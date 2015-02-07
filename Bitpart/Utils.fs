namespace Bitpart

module Utils =
    open System

    let [<Literal>] iso88591 = "iso-8859-1"

    let inline div n m =
        let (n,m) = if m < LanguagePrimitives.GenericZero then (-n,-m) else (n,m)
        (if n < LanguagePrimitives.GenericZero then (n - m + LanguagePrimitives.GenericOne) else n) / m

    let inline mod' n m = ((n % m) + m) % m

    let byteSeqToInt   (buffer:byte seq) = let b = buffer |> Seq.take 4 |> Seq.toArray in ((int b.[0] <<< 24) ||| (int b.[1] <<< 16) ||| (int b.[2] <<< 8) ||| int b.[3])
    let encoding = Text.Encoding.GetEncoding iso88591
    let stringToByteSeq (s:string) = encoding.GetBytes(s) :> seq<_>

    type outstate = IO.BinaryWriter
    type instate  = IO.BinaryReader

    let byteP (b:byte) (st:outstate) = st.Write(b)
    let byteU          (st:instate ) = st.ReadByte()

    let int16P (i:int16) (st:outstate) = st.Write(byte (i >>> 8)); st.Write(byte i)

    let int32P i (st:outstate) =
        byteP (byte (i >>> 24)) st
        byteP (byte (i >>> 16)) st
        byteP (byte (i >>>  8)) st
        byteP (byte i         ) st

    let int16U  (st:instate) = (int16 (st.ReadByte()) <<< 8) ||| int16 (st.ReadByte())

    let int32U st = (int (byteU st) <<< 24) ||| (int (byteU st) <<< 16) ||| (int (byteU st) <<< 8) ||| int (byteU st)

    let writeString (s:string) (st:outstate) = st.Write(encoding.GetBytes(s))
    let bytesToString b          = encoding.GetString(b)
    let stringToBytes (s:string) = encoding.GetBytes(s)

    let unixTimeToInt64 (y,m,d,s) =
        let m = (m + 9) % 12
        let y = y - m/10
        let days = int64 ((365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + (d-1) ) - 719468)
        days * 86400L + int64 s

    let int64ToUnixTime n =
        let getdays g' =
            let g  = 719468L+g'
            let y' = (10000L * g + 14780L)/3652425L
            let ddd' = g - (365L*y' + y'/4L - y'/100L + y'/400L)
            let y,ddd =
                if (ddd' < 0L) then                
                    let y = y'-1L in int y, int (g - (365L*y + y/4L - y/100L + y/400L))
                else int y',int ddd'
            let mi = int ( (100*ddd + 52)/3060 )
            let mm = (mi + 2)%12 + 1
            let y = y + (mi + 2)/12
            let dd = ddd - (mi*306 + 5)/10 + 1
            y, mm, dd
        let days = div  n 86400L
        let secs = int (mod' n 86400L)
        let (y,m,d) = getdays days
        (y,m,d, secs)

    let tupleToInt64 (nLo, nHi) = (int64 nHi <<< 32) ||| (int64 nLo &&& 4294967295L)
    let int64ToTuple (i:int64 ) = (int i, int (i >>> 32))


    let pickle valueP value =
        use stream = new IO.MemoryStream()
        use writer = new IO.BinaryWriter(stream)
        valueP value writer
        stream.ToArray()

    let unpickle valueU bytes =
        use stream = new System.IO.MemoryStream(bytes:byte[])
        use reader = new System.IO.BinaryReader(stream)
        valueU reader

    let inline (==) (a:string) (b:string) = a.Equals (b, StringComparison.OrdinalIgnoreCase)
    let inline (!=)  a = not << (==) a
    let (|CI|_|) str arg = if str == arg then Some () else None