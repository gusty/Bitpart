namespace Bitpart.Server

open System
open FSharpPlus

module Lib =

    let inline levenshteinDistance s t =
        if          s = t then 0
        elif length s = 0 then length t
        elif length t = 0 then length s
        else
            let v0, v1 = Array.init (length t + 1) id, Array.zeroCreate (length t + 1)
            for i = 0 to (length s)-1 do
                v1.[0] <- i + 1
                for j = 0 to (length t)-1 do
                    let cost = if item i s = item j t then 0 else 1
                    v1.[j + 1] <- v1.[j] + 1 |> min (v0.[j + 1] + 1) |> min (v0.[j] + cost)
                Array.blit v1 0 v0 0 v0.Length
            v1.[length t]

    let inline matchPercentage s t =
        let d = levenshteinDistance s t
        let l = max (length s) (length t)
        if d > l then 0.
        else (1. - (float d/ float l)) * 100.


type Counter (maxRepeats, interval, minMatchPercentage) =
    let arr = if interval > 0. then Array.zeroCreate<DateTime> maxRepeats else null
    let mutable arrIndex = 0
    let repetitions = ref 0
    let mutable lastMsg = [||] : byte []
    let eq x y =
        match minMatchPercentage with
        | 0.   -> true
        | 100. -> x = y
        | m    -> Lib.matchPercentage x y > m

    let reset () =
        match arr with
        | null -> repetitions := 0
        | _    -> Array.iteri (fun i _ -> arr.[i] <- minValue) arr

    let updAndCheck newTime =
        match arr with
        | null -> 
            incr repetitions
            !repetitions > maxRepeats            
        | _    ->
            let oldTime = arr.[arrIndex]
            arr.[arrIndex] <- newTime
            arrIndex <- (arrIndex + 1) % maxRepeats
            (newTime - oldTime).TotalMilliseconds < interval

    member __.Update (msgcontent, time) =
        if not (eq msgcontent lastMsg) then
            reset ()
            if minMatchPercentage = 100. then lastMsg <- msgcontent
        if minMatchPercentage < 100. && minMatchPercentage > 0. then lastMsg <- msgcontent
        updAndCheck time