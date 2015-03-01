namespace FsParsec

/// This is basically an FParsec wrapper compatible with FsControl
type Parser<'u, 'a> = Parser of (FParsec.CharStream<'u> -> FParsec.Reply<'a>)

[<AutoOpenAttribute>]
module Parser =

    open FParsec

    let run    (Parser p) s = 
        match run p s with
        | Success (res, _, _) -> Choice1Of2 res
        | Failure (err, _, _) -> Choice2Of2 err
    let unwrap (Parser p) = p

    let preturn x = Parser (preturn x)
    let pzero = Parser pzero

    let (>>%)   (Parser x)         b  = Parser (x >>% b)
    let (>>.)   (Parser x) (Parser y) = Parser (x >>. y)
    let (.>>)   (Parser x) (Parser y) = Parser (x .>> y)
    let between (Parser a) (Parser b) (Parser c) = Parser (between a b c)

    let pipe2  (Parser a) (Parser b)                                  f = Parser (pipe2 a b       f)    
    let pipe3  (Parser a) (Parser b) (Parser c)                       f = Parser (pipe3 a b c     f)
    let pipe4  (Parser a) (Parser b) (Parser c) (Parser d)            f = Parser (pipe4 a b c d   f)
    let pipe5  (Parser a) (Parser b) (Parser c) (Parser d) (Parser e) f = Parser (pipe5 a b c d e f)

    let choice x = Parser (choice (Seq.map unwrap x))
    let attempt (Parser x) = Parser (attempt x)
    
    let notFollowedBy (Parser p) = Parser (notFollowedBy p)

    let tuple2 (Parser a) (Parser b)                                  = Parser (tuple2 a b      )
    let tuple3 (Parser a) (Parser b) (Parser c)                       = Parser (tuple3 a b c    )
    let tuple4 (Parser a) (Parser b) (Parser c) (Parser d)            = Parser (tuple4 a b c d  )
    let tuple5 (Parser a) (Parser b) (Parser c) (Parser d) (Parser e) = Parser (tuple5 a b c d e)

    let sepBy        (Parser a) (Parser b) = Parser (sepBy        a b)
    let stringsSepBy (Parser a) (Parser b) = Parser (stringsSepBy a b)
    let manySatisfy f = Parser (manySatisfy f)
    let many1Satisfy2L a b c = Parser (many1Satisfy2L a b c)    

    let spaces    = Parser spaces
    let pstring s = Parser (pstring s)
    let anyOf   s = Parser (anyOf s)
    let hex = Parser hex    
    let stringCIReturn s a = Parser (stringCIReturn s a)
    let pstringCI s = Parser (pstringCI s)
    let pint32 = Parser pint32
    let pfloat = Parser pfloat
    let anyString x = Parser (anyString x)
    let eof = Parser eof
    let isLetter = isLetter
    let isDigit  = isDigit

    let createParserForwardedToRef() =
        let r = createParserForwardedToRef() |> fst |> Parser |> ref
        Parser (fun stream -> unwrap !r stream), r

open FsControl.Core.TypeMethods

type Parser with
    static member instance (_:Functor.Map       ,   Parser x,   _) = fun f -> Parser (FParsec.Primitives.(|>>) x f)
    static member instance (_:Applicative.Pure  , _:Parser<_,_>  ) = fun x -> Parser.preturn x
    static member instance (_:Monad.Bind    ,Parser x, _         ) = fun f -> Parser (FParsec.Primitives.(>>=) x (f >> Parser.unwrap)):Parser<'s,'b>
    static member instance (_:Functor.Zero, _:Parser<'s,'a>      ) = fun () -> Parser.pzero
    static member instance (_:Functor.Plus,   Parser x,   _      ) = fun (Parser y) -> Parser (FParsec.Primitives.(<|>) x y)