namespace Bitpart

open System
open System.Net
open System.Net.Sockets
open System.Configuration
open Bitpart.Log
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser

module Client =
      
    let printfn str = Printf.kprintf Console.WriteLine str   
    let printf  str = Printf.kprintf Console.Write     str

    let log level = logf "Bitpart.Client" level

    type SocketAvailable = Yes | No | Exn of exn
    let availableBytes (socket:Socket) =
        try  if (socket.Available > 0) then Yes else No
        with exn -> Exn exn

    let rec receive maxMessageSize (socket:Socket) =
        let decode msg =
            let unPackMessage (bytes:byte []) encKey = unpickle (messageU encKey) (Seq.toArray (seq {yield! (Seq.skip 6 bytes); yield 0uy; yield 0uy}) )
            try  Some (unPackMessage msg None)
            with exn -> log Error "Exception decoding message: %A" exn; None
        let rec receiveMsg size soFar =
            let ans = Array.create maxMessageSize 0uy
            match (try socket.Receive(ans, size, SocketFlags.None) with exn -> -1) with
            | -1 -> log Error "Exception while receiving from server."; [||]
            |  0 -> log Error "0 bytes received but %i bytes were requested." size; [||]
            |  n ->
                log Trace "received %i byte(s) from server" n
                let r = Array.concat [soFar; ans.[0..n-1]]
                if (r.Length < size) then receiveMsg size r else r

        log Trace "trying to get headers"
        let bytes = receiveMsg 6 [||]
        if bytes = [||] then None
        else
            let msgSize = byteSeqToInt bytes.[2..5]
            log Trace "Size is %i byte(s)" msgSize
            if not (bytes.[0..1] = [|114uy;0uy|]) then
                let n = socket.Receive (Array.create maxMessageSize 0uy)
                log Debug "Invalid headers, dropping 6 (headers) + %i byte(s)" n
                receive maxMessageSize socket
            elif (bytes.[6..].Length < msgSize) then 
                log Trace "Getting the remaining part."
                let rem = receiveMsg msgSize bytes.[6..]
                if rem = [||] then None
                else decode (Array.concat [bytes.[..5]; rem])
            else decode bytes


    let connectToNetServer (socket:Socket) (userName, password, ip, port, movieName, mode, encryptionKey) =
        log Debug "Trying to connect to %s:%i" ip port
        try
            socket.Connect (IPEndPoint (IPAddress.Parse ip, port))
            let msg = {
                        errorCode  = 0
                        timeStamp  = 0 
                        subject    = "Logon"
                        sender     = userName
                        recipients = [""]
                        content    = toLList [movieName; userName; password] |> pickle valueP
                      }
            packMessage (Some encryptionKey) msg |> socket.Send |> ignore
            None
        with exn -> Some exn

    let sendMessage userName (socket:Socket) (recipient, subject, content) =
        let msg = {
                    errorCode  = 0
                    timeStamp  = 0
                    subject    = subject
                    sender     = userName
                    recipients = recipient
                    content    = content
                  }
        try 
            let numberOfBytes = socket.Send (packMessage None msg)
            None
        with exn -> Some exn

    let sendNetMessage userName (socket:Socket) (cmd, subject) = sendMessage userName socket ([cmd], subject, pickle valueP LVoid)
    type Server = {id : int ; socket:Socket ; details : string * int * string}


    [<EntryPoint>]
    let main argv = 

        let nvc = ConfigurationManager.GetSection "servers" :?> Collections.Specialized.NameValueCollection

        let servers = nvc.AllKeys |> Seq.toArray |> Array.mapi (fun i k ->
            let s = nvc.[k]
            let p = s.IndexOf (';')
            let ipPort, encKey = s.[..p-1], s.[p+1..]
            match ipPort.Split (':') with
            | [|ip;port|] ->
                k , ip, Int32.Parse port , s.[p+1..]
            | _ -> failwith "Ip and Port format should be like this: 127.0.0.1:1626")        

        let debug = Boolean.Parse ConfigurationManager.AppSettings.["Debug"]
        Enum.Parse (typeof<LogLevel>, ConfigurationManager.AppSettings.["FileLogLevel"]) :?> LogLevel |> setFileLogLevel
        let maxMessageSize  = Int32.Parse ConfigurationManager.AppSettings.["MaxMessageSize"]
        let adminMovie = ConfigurationManager.AppSettings.["MovieName"]

        let log level str =
            if debug = true then logf "Bitpart.Client" level str
            else
                let f (str:string) = if level > Debug then printfn "%s" str
                Printf.kprintf f str    

        Console.ForegroundColor <- ConsoleColor.Gray
        printfn "Select Server"
        servers |> Seq.iteri (fun i (a,b,c,_) -> printfn "#%i %s (%s:%i)" i a b c)

        let serverID = 
            Seq.initInfinite (fun _ -> Console.ReadKey().KeyChar |> string |> Int32.TryParse)
                |> Seq.pick (function (true, i) -> Some i | _ -> printfn "\nPlease hit a number."; None)
                  
        let (serverName, ip, port, encKey) = servers.[serverID]
        printfn "\nServer %s" serverName    

        let socket = new Socket (AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.IP)
        let usrName = Guid.NewGuid().ToString("N").[..11].ToUpper() + "usrAdmin"
        let _movies = ref []

        let hold () =
            let rec loop () =
                try
                    let cmd = Console.ReadKey().KeyChar
                    let cnt,cnf = 
                        match cmd with
                        | 'M' -> 
                            printfn " -> Move to Net Server"
                            if (Seq.length !_movies < 2) then
                                printfn "No movies"
                                LPropList [], false
                            else 
                                let allMovies = "AllMovies"
                                let movies = allMovies :: !_movies |> Seq.zip (seq {0..Int32.MaxValue})
                                movies |> Seq.iter (fun (i,m) -> printfn "(%i) %s" i m)
                                printf "Movie #:"
                                let input = Console.ReadLine()
                                let movieId input = Seq.skip (Int32.Parse input) movies |> Seq.head |> snd
                                let movieIds = input.Split([|','|]) |> Seq.map movieId
                                printf "IP:"
                                let ip = Console.ReadLine()
                                printf  "Port:"
                                let port = Int32.Parse(Console.ReadLine())
                                printf  "Confirm moving movie:%s to %s:%i (y/n) :" (String.Join(", ", movieIds)) ip port
                                if Console.ReadKey().KeyChar = 'y' then
                                    Console.WriteLine()
                                    LPropList ["movies", toLList movieIds; "ip", LString ip; "port", LInteger port; "delay", LInteger 10000], true
                                else 
                                    printfn "\nCanceled"
                                    LPropList [], false
                    
                        | c   -> LPropList [], true
                    if cnf then 
                        let x = sendMessage usrName socket ([cmd.ToString() + "@" + adminMovie], "adminCmd", pickle valueP cnt)
                        ()
                    ()
                with exn -> printfn "%A" exn
                loop ()
            loop ()


        let receiveMsg () =
            log Trace "Entering receive loop from server."
            match receive maxMessageSize socket with
            | None         -> log Warn "Connection for server appears to be closed."; Some (new exn "Connection closed.")
            | Some message ->
                log Debug "Message received from server. Sender : %s - Subject : %s - ErrCode: %i - Rcpt: %A" message.sender message.subject message.errorCode message.recipients
                match message.subject with 
                | "Admin Command Reply" ->
                   match (unpickle valueU message.content) with
                   | LString s -> log Info "%s" s
                   | _         -> log Warn "Unknown reply format."
                   None
                | "getMovies" ->
                   match (unpickle valueU message.content) with
                   | LList lst -> _movies := lst |> Seq.choose (function (LString s) -> Some s | _ -> None) |> Seq.toList
                   | _         -> log Warn "Unknown reply format."
                   None
                | _ -> None


        let rec connLoop =
            let rec loop status = async {
                match status with
                | Some exn ->
                    log Error "Exception while connecting: %A" exn
                    socket.Dispose()
                    return ()
                | None ->            
                    let r = receiveMsg ()
                    return! loop r }
            loop (connectToNetServer socket (usrName, "PassWord", ip, port, adminMovie, 0, encKey ))

        Async.Start connLoop

        let getMovies _ =
            sendMessage usrName socket (["system.server.getMovies"], "getMovies", pickle valueP LVoid)
            |> ignore

        let timer = new Timers.Timer 2000.
        timer.Elapsed.Add getMovies
        timer.Enabled <- true

        hold()