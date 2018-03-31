namespace Bitpart.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharpPlus
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser
open Bitpart.Server
open System.Text.RegularExpressions
open System

[<TestClass>]
type Antiflood() =

    let check rule time msg = rule.Counter.Update(msg.content, time)

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

        
        let check = check rule
        let time = System.DateTime.Now
        Assert.IsFalse(check (time                                  ) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  100.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  200.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  300.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  400.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  500.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  600.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  700.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  800.) msg2) //9th msg, but content changed
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  900.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2000.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2100.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2200.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2300.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2400.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2500.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2600.) msg2) // 9th msg, but time OK
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds 2700.) msg2) //10th msg, but time OK
        Assert.IsTrue (check (time + TimeSpan.FromMilliseconds 2800.) msg2) //11th msg, time less than interval
 


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

        let check = check rule
        let time = System.DateTime.Now

        Assert.IsFalse(check (time                                  ) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  100.) msg1) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  200.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  300.) msg2) 
        Assert.IsFalse(check (time + TimeSpan.FromMilliseconds  400.) msg2) 
        Assert.IsTrue (check (time + TimeSpan.FromMilliseconds 2800.) msg2) //6th msg 90% equal