namespace Bitpart

open System
open FSharpPlus
open FSharpPlus.Math.Generic

module Utils =   

    let encoding = Text.Encoding.GetEncoding "iso-8859-1"

    type outstate = IO.BinaryWriter
    type instate  = IO.BinaryReader

    let byteP (b:byte) (st:outstate) = st.Write b
    let byteU          (st:instate ) = st.ReadByte ()

    let inline numP (num: 'T) (st:outstate) = st.Write (toBytesBE num)
    let inline numU (st:instate) = ofBytesBE (st.ReadBytes sizeof<'T>) :'T

    let writeString   (s:string) (st:outstate) = s |> encoding.GetBytes |> st.Write

    let unixTimeToInt64 (y, m, d, s) =
        let m = (m + 9) % 12
        let y = y - m/10
        let days = int64 ((365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + (d-1)) - 719468)
        days * 86400L + int64 s

    let int64ToUnixTime n =
        let getdays g' =
            let g  = 719468L+g'
            let y' = (10000L * g + 14780L) / 3652425L
            let ddd' = g - (365L*y' + y'/4L - y'/100L + y'/400L)
            let y, ddd =
                if ddd' < 0L then                
                    let y = y'-1L in int y, int (g - (365L*y + y/4L - y/100L + y/400L))
                else int y', int ddd'
            let mi = int ((100*ddd + 52) / 3060)
            let mm = (mi + 2) % 12 + 1
            let y = y + (mi + 2) / 12
            let dd = ddd - (mi*306 + 5) / 10 + 1
            y, mm, dd
        let days, secs = divRemE n 86400L
        let y, m, d = getdays days
        (y,m,d, int secs)

    let tupleToInt64 (nLo, nHi) = (int64 nHi <<< 32) ||| (int64 nLo &&& 4294967295L)
    let int64ToTuple (i:int64 ) = (int i, int (i >>> 32))


    let pickle valueP value =
        use stream = new IO.MemoryStream ()
        use writer = new IO.BinaryWriter (stream)
        valueP value writer
        stream.ToArray ()

    let unpickle valueU (bytes:byte[]) =
        use stream = new IO.MemoryStream (bytes)
        use reader = new IO.BinaryReader (stream)
        valueU reader

    let inline (==) (a:string) (b:string) = a.Equals (b, StringComparison.OrdinalIgnoreCase)
    let inline (!=)  a = not << (==) a
    let (|CI|_|) str arg = if str == arg then Some () else None