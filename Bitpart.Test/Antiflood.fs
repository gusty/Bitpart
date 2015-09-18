namespace Bitpart.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsControl.Operators
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser
open Bitpart.Server
open System.Text.RegularExpressions
open System

[<TestClass>]
type Antiflood() =
    [<TestMethod>]        
    member this.Max8repetitionsInLessThan1sec() =
        let sbj, repetitions, time, mtch = "chat", 8, 1000., 100.
        let rule = {RuleId = "max8repetitionsInLessThan1sec"; Counter = Counter(repetitions, time, mtch); Regex = Regex(Regex.Escape(sbj).Replace(@"\*", ".*").Replace(@"\?", "."), RegexOptions.Singleline ||| RegexOptions.Compiled)}
        let msg1 = 
            {
                sender      = "sender1"
                recipients  = ["recípient1";"recípient2";"@AllUsers"]
                subject     = "chat"
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

        let msg2 = 
            {
                sender      = "sender2"
                recipients  = ["@AllUsers"]
                subject     = "chat"
                content     = 
                    pickle valueP (
                        LPropList [
                            "move"  , LList [LRect(LInteger 2, LFloat -2879.54, LInteger -7984, LFloat 1.1); LDate(2012,2,28,34786)]
                            "object", LString "Queen"
                            "Color" , LColor (255,0,0)
                            "img"   , LPicture [|12uy;24uy;24uy|]
                                ])
                errorCode   = -1024
                timeStamp   = 2015
            }

        let mutable finished = false
        let mutable passed = false
        let state = new AntiFlood()
        let fail rule =
            finished <- true
        let pass r =
            passed   <- true
            finished <- true
        let time = System.DateTime.Now
        state.post (time                                  ) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  100.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  200.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  300.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  400.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  500.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  600.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  700.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  800.) [rule] msg2 ignore fail //9th msg, but content changed
        state.post (time + TimeSpan.FromMilliseconds  900.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2000.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2100.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2200.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2300.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2400.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2500.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2600.) [rule] msg2 ignore fail // 9th msg, but time OK
        state.post (time + TimeSpan.FromMilliseconds 2700.) [rule] msg2 ignore fail //10th msg, but time OK
        state.post (time + TimeSpan.FromMilliseconds 2800.) [rule] msg2 fail   pass //11th msg, time less than interval
        while not finished do ()
        Assert.IsTrue(passed)


    [<TestMethod>]        
    member this.Max5repetitionsSimilarMessage() =
        let sbj, repetitions, time, mtch = "chat", 5, 0., 90.
        let rule = {RuleId = "max8repetitionsInLessThan1sec"; Counter = Counter(repetitions, time, mtch); Regex = Regex(Regex.Escape(sbj).Replace(@"\*", ".*").Replace(@"\?", "."), RegexOptions.Singleline ||| RegexOptions.Compiled)}
        let msg1 = 
            {
                sender      = "sender1"
                recipients  = ["recípient1";"recípient2";"@AllUsers"]
                subject     = "chat"
                content     = 
                    pickle valueP (
                        LPropList [
                            "object", LString "this is a test"
                            "Color" , LColor (255,0,55)
                                ])
                errorCode   = -1024
                timeStamp   = 2015
            }

        let msg2 = 
            {
                sender      = "sender2"
                recipients  = ["@AllUsers"]
                subject     = "chat"
                content     = 
                    pickle valueP (
                        LPropList [
                            "object", LString "tis is a tes"
                            "Color" , LColor (255,0,0)
                                ])
                errorCode   = -1024
                timeStamp   = 2015
            }

        let mutable finished = false
        let mutable passed = false
        let state = new AntiFlood()
        let fail rule =
            finished <- true
        let pass r =
            passed   <- true
            finished <- true
        let time = System.DateTime.Now
        state.post (time                                  ) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  100.) [rule] msg1 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  200.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  300.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds  400.) [rule] msg2 ignore fail
        state.post (time + TimeSpan.FromMilliseconds 2800.) [rule] msg2 fail   pass //6th msg 90% equal
        while not finished do ()
        Assert.IsTrue(passed)