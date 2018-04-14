module Bitpart.Multiuser
 
open System
open FSharpPlus
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler

type Message = 
    {
        errorCode : int
        timeStamp : int
        subject   : string
        sender    : string
        recipients: string list
        content   : byte []
    }

let inline writeMessage (sender, rcpts, subject, content) =
    {
        errorCode  = 0
        timeStamp  = 0
        subject    = subject
        sender     = sender
        recipients = toList rcpts
        content    = content
    }

let prettyPrintMsg msg =
    let rcpt    = toLList msg.recipients
    let content = unpickle valueU msg.content
    sprintf "Sender: %s - Subject: %s - ErrorCode: %i - rcpt: %A - content: %A" msg.sender msg.subject msg.errorCode rcpt content


let messageP encryptionKey msg st =
    numP  msg.errorCode st
    numP  msg.timeStamp st
    stringP msg.subject st 
    stringP msg.sender  st
    numP (length msg.recipients) st
    iter (fun recipient -> stringP recipient st) msg.recipients
    st.Write (match encryptionKey with Some key -> Blowfish.encode key msg.content | _ -> msg.content)

let messageU encryptionKeyVectors st =
    {
        errorCode  = numU    st
        timeStamp  = numU    st
        subject    = stringU st
        sender     = stringU st
        recipients = listU   stringU st   
        content    = 
            let ct = st.ReadBytes (int (st.BaseStream.Length - st.BaseStream.Position))
            match encryptionKeyVectors with None -> ct | Some v -> Blowfish.blowfish v ct
    }

let packMessage encKey message =
    let msgBody = pickle (messageP encKey) message
    use stream  = new IO.MemoryStream ()
    use writer  = new IO.BinaryWriter (stream)
    byteP  114uy writer
    byteP    0uy writer
    numP (length msgBody) writer
    writer.Write msgBody
    stream.ToArray ()