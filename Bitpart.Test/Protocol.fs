namespace Bitpart.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsControl.Operators
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser

[<TestClass>]
type Protocol() =
    [<TestMethod>]        
    member this.PickleUnpickle() =
        let msg1 = 
            {
                sender      = "TheSender_ñéè"
                recipients  = ["recípient1";"recípient2";"@AllUsers"]
                subject     = "NewMessage"
                content     = 
                    pickle valueP (
                        LPropList [
                            "move"  , LList [LRect(LInteger 2, LFloat -2879.54, LInteger -7984, LFloat 1.1); LDate(2012,2,28,34786)]
                            "object", LString "Queen"
                            "Color" , LColor (255,0,55)
                            "img"   , LPicture [|12uy;24uy;24uy|]
                                ])
                errorCode   = -1024
                timeStamp   = 2015
            }

        let bytes = pickle   (messageP None) msg1
        let msg2  = unpickle (messageU None) bytes
        Assert.AreEqual (msg1, msg2)

    [<TestMethod>]   
    member this.PickleUnpickleTime() =
        let s = System.DateTime.Now
        map_ (fun _ -> this.PickleUnpickle()) {1..40000}
        Assert.IsTrue ((System.DateTime.Now - s).TotalMilliseconds < 1000.)