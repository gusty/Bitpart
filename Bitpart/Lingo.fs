namespace Bitpart

open System
open FsControl.Operators
open Bitpart.Utils

module Lingo =

    type LValue =
        | LVoid
        | LSymbol    of string
        | LString    of string
        | LFloat     of float
        | LInteger   of int
        | LPoint     of LValue * LValue
        | LRect      of LValue * LValue * LValue * LValue
        | LColor     of int    * int    * int
        | LVector    of single * single * single
        | LTransform of single * single * single * single * single * single * single * single * single * single * single * single * single * single * single * single
        | LDate      of int * int * int * int
        | LList      of LValue list
        | LPropList  of (string*LValue) list
        | LPicture   of byte []
        | LMedia     of byte []
        | LRaw       of byte []
        with override t.ToString() =
                match t with
                | LVoid      -> "void"
                | LSymbol  s -> "#" + s 
                | LString  s -> "\"" + s + "\""
                | LFloat   f -> string f
                | LInteger i -> string i
                | LPoint   (x, y)    -> sprintf "point(%A, %A)" x y
                | LRect (a, b, c, d) -> sprintf "rect(%A, %A, %A, %A)" a b c d
                | LColor   (r, g, b) -> sprintf "color(%i, %i, %i)"  r g b
                | LVector  (x, y, z) -> sprintf "vector(%f, %f, %f)" x y z
                | LList     l -> "[" + String.concat ", " (l |>> (box >> string))  + "]"
                | LPropList m -> "[" + String.concat ", " (m |>> fun (k, v) -> "#" + k + ": " + string (box v))   + "]"
                | LDate (y,m,d,s) -> sprintf "date (%i, %i, %i) + %i seconds" y m d s
                | LTransform (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) ->
                    sprintf  "transform(%f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f)" a b c d e f g h i j k l m n o p
                | LPicture b -> sprintf "Picture Data: %i bytes long %A" (length b) (take 8 b |>> string)
                | LMedia   b -> sprintf "Media Data: %i bytes long %A"   (length b) (take 8 b |>> string)
                | LRaw     b -> sprintf "Raw Data: %i bytes long %A"     (length b) (take 8 b |>> string)
 

    module Type =
        let [<Literal>] Void      =  0s
        let [<Literal>] Integer   =  1s
        let [<Literal>] Symbol    =  2s
        let [<Literal>] String    =  3s
        let [<Literal>] Picture   =  5s
        let [<Literal>] Float     =  6s
        let [<Literal>] List      =  7s
        let [<Literal>] Point     =  8s
        let [<Literal>] Rect      =  9s
        let [<Literal>] PropList  = 10s
        let [<Literal>] Color     = 18s
        let [<Literal>] Date      = 19s
        let [<Literal>] Media     = 20s
        let [<Literal>] Vector    = 22s
        let [<Literal>] Transform = 23s


    module Pickler =
        open Type
        let chunkP (bytes:byte []) st =
            let len = length bytes
            numP len st
            st.Write bytes
            if len % 2 = 1 then st.Write 0uy

        let chunkU st =
            let n = numU st
            let res = st.ReadBytes n
            if n % 2 = 1 then st.ReadByte () |> ignore
            res

        let stringP (s:string) = chunkP (encoding.GetBytes s)
        let stringU st = encoding.GetString (chunkU st)

        let dateP dt st = 
            let lo, hi = int64ToTuple (unixTimeToInt64 dt)
            numP lo st
            numP hi st

        let dateU st =
            let lo = numU st
            let hi = numU st
            (lo, hi) |> tupleToInt64 |> int64ToUnixTime

        let listU f st = {1..numU st} |> foldl (fun elems _ -> f st :: elems) [] |> rev

        let rec valueP value st =
            match value with
            | LVoid              -> numP Void    st
            | LInteger int       -> numP Integer st; numP    int     st
            | LFloat   float     -> numP Float   st; numP    float   st
            | LSymbol  string    -> numP Symbol  st; stringP string  st
            | LString  string    -> numP String  st; stringP string  st
            | LList    list      -> numP List    st; numP (length list) st; map_ (fun e -> valueP e st) list
            | LPoint   (x, y)    -> numP Point   st; valueP  x st; valueP y st
            | LRect (a, b, c, d) -> numP Rect    st; valueP  a st; valueP b st; valueP c st; valueP d st
            | LVector  (x, y, z) -> numP Vector  st; numP x st; numP y st; numP z st
            | LTransform (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd, pe, pf)  -> 
                numP Transform st
                numP p0 st; numP p1 st; numP p2 st; numP p3 st; numP p4 st; numP p5 st; numP p6 st; numP p7 st
                numP p8 st; numP p9 st; numP pa st; numP pb st; numP pc st; numP pd st; numP pe st; numP pf st
            | LPropList list  -> numP PropList st; numP (length list) st; map_ (fun (k, v) -> numP Symbol st; stringP k st; valueP v st) list
            | LColor  (r,g,b) -> numP Color   st; byteP 1uy st; byteP (byte r) st; byteP (byte g) st; byteP (byte b) st;
            | LDate (y,m,d,s) -> numP Date    st; dateP (y, m, d, s)  st
            | LPicture bytes  -> numP Picture st; chunkP bytes st
            | LMedia   bytes  -> numP Media   st; chunkP bytes st
            | LRaw     bytes  -> st.Write (toArray bytes)

        let rec valueU st =
            let ltype = numU st
            let header lValue = [|byte (lValue >>>  8); byte lValue|]
            match ltype with
            | Void      -> LVoid
            | Integer   -> LInteger   (numU  st)
            | Float     -> LFloat     (numU st)
            | Symbol    -> LSymbol    (stringU st)
            | String    -> LString    (stringU st)
            | List      -> LList      (listU valueU st)
            | Point     -> LPoint     (valueU  st, valueU  st)
            | Rect      -> LRect      (valueU  st, valueU  st, valueU  st, valueU  st)
            | Vector    -> LVector    (numU st, numU st, numU st)
            | Transform -> LTransform (
                            numU st, numU st, numU st, numU st, numU st, numU st, numU st, numU st, 
                            numU st, numU st, numU st, numU st, numU st, numU st, numU st, numU st)
            | PropList  ->
                let f st = (match valueU st with LSymbol s -> s | _ -> failwith "Symbol expected."), valueU st
                LPropList (listU f st)
            | Color     -> ignore   (byteU  st); LColor (int (byteU st), int (byteU st), int (byteU st))
            | Date      -> LDate    (dateU  st)
            | Picture   -> LPicture (chunkU st)
            | Media     -> LMedia   (chunkU st)
            | _         -> LRaw     (header ltype <|> st.ReadBytes (int (st.BaseStream.Length-st.BaseStream.Position)))


    let inline toLList v = v |>> LString  |> toList |> LList
    
    open FParsec
    module internal Parser =
        
        let parse parser str =
            match run parser str with
            | Success(result  , _, _) -> Choice1Of2 result
            | Failure(errorMsg, _, _) -> Choice2Of2 errorMsg
    

        // some abbreviations
        let internal ws    = spaces // eats any whitespace
        let internal str s = pstring s

        let stringLiteral =
            let escape = anyOf "\"\\/bfnrt" |>> function
                                                | 'b' -> "\b"
                                                | 'f' -> "\u000C"
                                                | 'n' -> "\n"
                                                | 'r' -> "\r"
                                                | 't' -> "\t"
                                                | c   -> string c // every other char is mapped to itself
            let unicodeEscape =
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )
            between (str "\"") (str "\"") (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\')) (str "\\" >>. (escape <|> unicodeEscape)))

        let lstring = stringLiteral |>> LString

        let pIdentifier =
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_'
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

        let internal btParens p = (between (str "(") (str ")")) p
        let internal i0 p =             ws >>. p .>> ws
        let internal iN p = str "," >>. ws >>. p .>> ws

        let pSymbol = str "#" >>. pIdentifier
        let lsymbol = pSymbol |>> LSymbol
        let lfloat = (str "." >>. pint32 |>> (string >> (+) "0." >> System.Double.Parse >> LFloat)) <|> (pfloat |>> LFloat)
        let linteger = pint32 .>> notFollowedBy (pstring ".") |>> LInteger
        let ltrue  = stringCIReturn "true"  (LInteger 1)
        let lfalse = stringCIReturn "false" (LInteger 0)
        let lvoid  = stringCIReturn "void" LVoid
        let lvoidB = stringCIReturn "<void>" LVoid

        let lpoint  p1 p2       = pstringCI "point"  >>. ws >>. (btParens <| tuple2 (i0 p1) (iN p2)                    ) |>> LPoint
        let lcolor  p1 p2 p3    = pstringCI "color"  >>. ws >>. (btParens <| tuple3 (i0 p1) (iN p2) (iN p3)            ) |>> LColor
        let lvector p1 p2 p3    = pstringCI "vector" >>. ws >>. (btParens <| tuple3 (i0 p1) (iN p2) (iN p3)            ) |>> LVector
        let ldate   p1 p2 p3    = pstringCI "date"   >>. ws >>. (btParens <| tuple4 (i0 p1) (iN p2) (iN p3) (preturn 0)) |>> LDate
        let lrect   p1 p2 p3 p4 = pstringCI "rect"   >>. ws >>. (btParens <| tuple4 (i0 p1) (iN p2) (iN p3) (iN p4)    ) |>> LRect

        let lvalue, lvalueRef = createParserForwardedToRef()

        let listBtStrings sOpen sClose pElement f = between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)
        let emptyplist = str "[" >>. ws >>. str ":" >>. ws >>. str "]" >>% (LPropList [])
        let keyValue = tuple2 pSymbol (ws >>. str ":" >>. ws >>. lvalue)
        let llist  = listBtStrings "[" "]" lvalue   LList
        let lplist = listBtStrings "[" "]" keyValue LPropList
        let psingle x = (pfloat |>> single) x
        let number = attempt linteger <|> lfloat

        do lvalueRef := choice [attempt llist
                                attempt lplist
                                emptyplist
                                lstring
                                lsymbol
                                lpoint  number  number
                                lrect   number  number  number  number
                                lcolor  pint32  pint32  pint32
                                ldate   pint32  pint32  pint32
                                lvector psingle psingle psingle                            
                                number
                                ltrue
                                lfalse
                                lvoid
                                lvoidB]

        let plingoValue = ws >>. lvalue .>> ws .>> eof

    type LValue with
        
        static member TryParse (s:string, [<Runtime.InteropServices.Out>]result: LValue byref) =
            match run Parser.plingoValue s with
            | Success (res, _, _) -> result <- res; true
            | Failure (err, _, _) -> false

        static member Parse (s:string, culture:Globalization.CultureInfo) =
            match run Parser.plingoValue s with
            | Success (res, _, _) -> res
            | Failure (err, _, _) -> FormatException err |> raise