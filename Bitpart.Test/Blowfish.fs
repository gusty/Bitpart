namespace Bitpart.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharpPlus

[<TestClass>]
type Blowfish () =    
    let bfkey  = 
        [| 
            73uy;   80uy;  65uy; 100uy; 100uy; 114uy; 101uy; 115uy; 115uy;  32uy;
            114uy; 101uy; 115uy; 111uy; 108uy; 117uy; 116uy; 105uy; 111uy; 110uy;
        |]

    let target = Bitpart.Blowfish.blowfishCypher bfkey

    [<TestMethod>]        
    member this.Decrypt () =
        let buffer =
            [|
                140uy; 176uy;  97uy; 202uy;
                 17uy;  83uy; 160uy;  87uy;
                248uy; 108uy; 216uy; 200uy;
                 10uy;  79uy; 147uy;  72uy;
                 99uy; 217uy; 105uy; 108uy;
                 74uy;  27uy;  76uy;  59uy;
                165uy; 198uy;  52uy;  53uy;
                211uy; 218uy;  65uy;  29uy;
                90uy;    8uy; 128uy; 113uy;
                171uy;  83uy; 131uy;   9uy;
            |]
        Bitpart.Blowfish.encrypt_decrypt target buffer

        let assertion =
            (buffer.[0]  =   0uy) &&
            (buffer.[1]  =   7uy) &&
            (buffer.[2]  =   0uy) &&
            (buffer.[3]  =   0uy) &&
            (buffer.[4]  =   0uy) &&
            (buffer.[5]  =   3uy) &&
            (buffer.[6]  =   0uy) &&
            (buffer.[7]  =   3uy) &&
            (buffer.[8]  =   0uy) &&
            (buffer.[9]  =   0uy) &&
            (buffer.[37] = 111uy) &&
            (buffer.[38] = 114uy) &&
            (buffer.[39] = 100uy)
        Assert.IsTrue  assertion

    [<TestMethod>]   
    member this.DecryptKeyBlockTest () =
        let lCipherBlock = 203447117607259685L
        let expected = -2567789888536546563L
        let actual = Bitpart.Blowfish.decryptBlock target lCipherBlock
        assert (expected = actual)

    [<TestMethod>]
    member this.DecryptTime () =
        let s = System.DateTime.Now
        {1..100000} |> iter (fun _ ->
            let dec, decblock = this.Decrypt (), this.DecryptKeyBlockTest ()
            ())
        #if DEBUG
        Assert.IsTrue  ((System.DateTime.Now - s).TotalMilliseconds < 2000.)
        #else
        Assert.IsTrue  ((System.DateTime.Now - s).TotalMilliseconds < 400.)
        #endif