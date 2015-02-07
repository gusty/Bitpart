namespace Bitpart.Server

open System
open System.Configuration
open System.Data.SqlClient
open SuperSocket.SocketBase
open SuperSocket.SocketBase.Protocol
open SuperSocket.Facility.Protocol
open Bitpart.Log
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser

module internal LogTemp = 
    let [<Literal>] logName = "Bitpart.Server"
    let log level = Bitpart.Log.logf logName level
    Bitpart.Log.setLogFileName "Server-log"
open LogTemp

type User = {Name: string; Movie: string; Groups: string list; Session: string}

type ServerMessage =
    | Command  of (User list -> User -> User List)
    | Function of (User -> LValue)    
    | AdminCmd of string * (string * LValue) seq
    | DBCmd    of (string * LValue) seq
    | UserMsg  of (string option * string option) list
    | Invalid

type MbxMessage =
    | ServerCmd of (User list -> User        -> User List)
    | LogOnOff  of (User list -> User option -> User List)

type State() =
    let mutable users = []
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop() = async {
            let! (session, cmd) = inbox.Receive()
            users <-
                match cmd, (List.tryFind (fun {Session = s} -> s == session) users) with
                | LogOnOff  f, sender      -> f users sender
                | ServerCmd f, Some sender -> f users sender
                | ServerCmd f, None        -> log Error "Session not found: %s." session; users
            return! (loop())}
        loop())
    do agent.Error.Add(log Fatal "Terminate execution due to unhandled exception in the MailboxProcessor. Exception was: %A")
    member this.Users              with get() = users
    member this.CurrentQueueLength with get() = agent.CurrentQueueLength
    member this.post sessionID mbxmsg = agent.Post(sessionID, mbxmsg)

[<AllowNullLiteral>]
type CustomProtocolSession() =
    inherit AppSession<CustomProtocolSession, BinaryRequestInfo>()
    let messageLogSize = 100
    let lastMessages   = Array.zeroCreate messageLogSize
    member val Authenticated   = false  with get, set
    member val ProtocolVersion = (0, 0) with get, set
    member val ClientVersion   = (0, 0) with get, set
    member this.CheckNewMessage msgId = 
        let  v = lastMessages.[msgId % messageLogSize]
        if   v = msgId then log Warn "Duplicate message: %i - Session: %s" msgId this.SessionID ; false
        elif v > msgId then log Warn "New messageId: %i is too old compared with previous one: %i - Session: %s" msgId v this.SessionID; false
        else lastMessages.[msgId % messageLogSize] <- msgId; true
    override this.HandleException(e) =
        log Fatal "Unhandled exception in session: %s : %A " this.SessionID e
        this.Close(CloseReason.ApplicationError)

type MyReceiveFilter(appServer,  appSession:CustomProtocolSession,  remoteEndPoint) =
    inherit FixedHeaderReceiveFilter<BinaryRequestInfo>(6)
    override this.GetBodyLengthFromHeader(header, offset, length) =
        if (not (header.[offset] = 114uy) || not (header.[offset + 1] = 0uy)) then            
            if appSession.Authenticated then 
                log Warn "Invalid headers: %i - %i received from %A , session: %s . Dropping." header.[offset] header.[offset+1] remoteEndPoint appSession.SessionID
                this.Reset()
            else
                log Warn "Invalid headers: %i - %i received from %A , session: %s . Client is not authenticated, will disconnect." header.[offset] header.[offset+1] remoteEndPoint appSession.SessionID
                appSession.Close(CloseReason.ProtocolError)
        byteSeqToInt(header.[offset+2..offset+5])
    override this.ResolveRequestInfo(header, bodyBuffer, offset, length) = 
        new BinaryRequestInfo(Text.Encoding.UTF8.GetString(header.Array, header.Offset, 2), SuperSocket.Common.BinaryUtil.CloneRange(bodyBuffer, offset, length))

type FilterFactory() =
    interface IReceiveFilterFactory<BinaryRequestInfo> with
        member this.CreateFilter(appServer,  appSession,  remoteEndPoint) =
            log Debug "New socket session: %s from %s" appSession.SessionID (appSession.RemoteEndPoint.Address.ToString())
            new MyReceiveFilter(appServer,  appSession:?> CustomProtocolSession,  remoteEndPoint) :> IReceiveFilter<BinaryRequestInfo>
    
type Protocol() = 
    inherit AppServer<CustomProtocolSession, BinaryRequestInfo>(new FilterFactory())   
    
    let [<Literal>] MvAdm = "MovieAdmin"
    let [<Literal>] MvSQL = "MovieSQL"

    let initialMinSLL, initialMinFLL = Info, Debug
    let state = State()
    member val BlackList         = []                              with get, set
    member val MaxUserCount      = (0, DateTime.MinValue)          with get, set
    member val MaxSessionCount   = (0, DateTime.MinValue)          with get, set
    member val MinScreenLogLevel = initialMinSLL                   with get, set
    member val MinFileLogLevel   = initialMinFLL                   with get, set
    member val MinLogLevel       = min initialMinSLL initialMinFLL with get, set
    member val blowfishVector    = ([||],[||],[||],[||],[||])      with get, set
    member val dbCnString        = None                            with get, set
    member val spStats           = None                            with get, set
    member val dbFnName          = "strFunction"                   with get, set
    member val dbFnArgs          = "lstArgs"                       with get, set
    member val dbFnResult        = "varResult"                     with get, set

    override appServer.Setup(rootConfig, config) =
        let encKey = config.Options.["encryptionKey"]
        appServer.blowfishVector <- (encKey |> stringToByteSeq |> Seq.toArray |> Bitpart.Blowfish.blowfishCypher)
        let toOption  (entry:string) = 
            let value = config.Options.[entry]
            if (System.String.IsNullOrEmpty value) then 
                log Info "No %s entry in config, this feature will be disabled." entry
                None 
            else Some value
            
        appServer.dbCnString <- toOption "dbCnString"
        appServer.spStats    <- toOption "spStats"
        appServer.dbFnName   <- defaultArg (toOption "dbFnName")   appServer.dbFnName
        appServer.dbFnArgs   <- defaultArg (toOption "dbFnArgs")   appServer.dbFnArgs
        appServer.dbFnResult <- defaultArg (toOption "dbFnResult") appServer.dbFnResult

        true
    
    override appServer.OnStartup() =

        let isInBlackList (s:IAppSession) = Seq.exists ((=) (s.RemoteEndPoint.Address.ToString())) appServer.BlackList
        let ipAddress     (s:IAppSession) = s.RemoteEndPoint.Address.ToString()

        Bitpart.Log.setLogFileName "Server-log"
        let setScreenLogLevel level =
            appServer.MinScreenLogLevel <- level
            appServer.MinLogLevel       <- min appServer.MinScreenLogLevel appServer.MinFileLogLevel
            Bitpart.Log.setScreenLogLevel level
        let setFileLogLevel level =
            appServer.MinFileLogLevel   <- level
            appServer.MinLogLevel       <- min appServer.MinScreenLogLevel appServer.MinFileLogLevel
            Bitpart.Log.setFileLogLevel level
        setScreenLogLevel appServer.MinScreenLogLevel
        setFileLogLevel   appServer.MinFileLogLevel

        let send message (session:CustomProtocolSession) =            
            if session = null then log Debug "Session does not exists anymore."
            else
                let bytes = packMessage None message
                if not (session.SocketSession.TrySend(new ArraySegment<_>(bytes))) then 
                    log Warn "Dropping Message to session: %s from %-15s , size was %-4i (sender = %s, rcpts = %A, subject = %s)." session.SessionID (ipAddress session) (Seq.length bytes) message.sender message.recipients message.subject


        let processDBCommand (session:CustomProtocolSession) plist =
            match appServer.dbCnString with
            | None   -> log Warn "execDbQuery received but no connection string specified: %A" plist
            | Some s ->
                let execDbQuery sp_name sp_args =
                    log Trace "execDbQuery %s %s" sp_name sp_args
                    use conn = new SqlConnection(s)
                    conn.Open()
                    let query = "exec sp_"+sp_name+" "+sp_args
                    use command = new SqlCommand(query, conn)
                    let lst = seq {
                        use reader = command.ExecuteReader()
                        while reader.Read() do
                            yield ([0.. reader.FieldCount-1] |> List.map (fun i -> 
                                if reader.GetFieldType(i) = typeof<Boolean> then if reader.GetBoolean(i) then "1" else "0" 
                                else reader.[i].ToString()
                                )) } |> Seq.toList
                    log Trace "DB-Execution results: %A" lst
                    lst
                
                let dct = dict plist
                match (dct.TryGetValue appServer.dbFnName , dct.TryGetValue appServer.dbFnArgs) with
                | (true, LString fn), (true, LList args) ->
                    log Trace "Msg Content is : %A" plist
                    let strArgs = String.concat "," (Seq.map (function 
                        | LString  e -> "'" + e.Replace("'","''") + "'" 
                        | LInteger e -> "'" + string e + "'" 
                        | LVoid -> "'" + "<Void>" + "'"
                        | s -> failwith (sprintf "Unexpected DBExec message format received: %A" s)) args)
                    let lstResult = execDbQuery fn strArgs
                    let result = (Seq.toList plist) @ [appServer.dbFnResult, lstResult |> List.map (List.map LString >> LList) |> LList]
                    log Trace "Result before sending to server: %A" (LPropList result)

                    let msg = writeMessage ("System", state.Users |> Seq.filter (fun {Session = s} -> s == session.SessionID) |> Seq.map (fun {Name = n} -> n), "DBexec", LPropList result |> pickle valueP)
                    session |> send msg

                | _ -> log Error "Wrong DBExec message content format: %A ." dct


        let processAdminCommand session cmd plist =
            let nl = Environment.NewLine
            let reply str = session |> send (writeMessage ("System", (state.Users |> Seq.filter (fun {Session = s} -> s == session.SessionID) |> Seq.map (fun {Name = n} -> n)), "Admin Command Reply", pickle valueP (LString str)))
            try
                match cmd with
                | "t" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Trace); setScreenLogLevel Trace
                | "d" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Debug); setScreenLogLevel Debug
                | "i" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Info ); setScreenLogLevel Info 
                | "w" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Warn ); setScreenLogLevel Warn 
                | "e" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Error); setScreenLogLevel Error
                | "f" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Fatal); setScreenLogLevel Fatal
                | "o" -> reply(sprintf " -> Changing minimum screen log Level to: %A" Off  ); setScreenLogLevel Off  
                | "T" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Trace); setFileLogLevel   Trace
                | "D" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Debug); setFileLogLevel   Debug
                | "I" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Info ); setFileLogLevel   Info 
                | "W" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Warn ); setFileLogLevel   Warn 
                | "E" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Error); setFileLogLevel   Error
                | "F" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Fatal); setFileLogLevel   Fatal
                | "O" -> reply(sprintf " -> Changing minimum file log Level to: %A"   Off  ); setFileLogLevel   Off  
                | "l" -> 
                    reply(sprintf " -> Loading and applying blacklist ...")
                    appServer.BlackList <- IO.File.ReadAllLines "blacklist.txt" |> Array.toList
                    appServer.GetAllSessions() |> Seq.filter isInBlackList |> Seq.iter (fun s -> 
                        log Warn "Session %s from %s is Blacklisted, will be disconnected immediately." s.SessionID (ipAddress s)
                        s.Close(CloseReason.ServerClosing))
                | "u" -> 
                    let users = state.Users
                    let detailed = users |> Seq.sort |> Seq.map (fun u -> 
                        let s = appServer.GetSessionByID(u.Session)
                        if s = null then "Session no longer exists."
                        else sprintf "Name: %-26s , Movie: %-22s , Session: %s , IP:%-15s , Groups: %A" u.Name u.Movie u.Session (ipAddress s) u.Groups)
                    let detail = String.Join(nl, detailed)
                    reply (sprintf " -> User Details:%s%s%sUser Count: %i" nl detail nl (List.length users))
                | "m" ->
                    let users = state.Users
                    let detail = String.Join(nl, users |> Seq.groupBy (fun {Movie = m} -> m.ToLowerInvariant()) |> Seq.map (fun (m, u) -> sprintf "Movie: %-22s , Users:%3i" m (Seq.length u)))
                    reply (sprintf " -> Movie Details:%s%s%sUser Count: %i" nl detail nl (List.length users))
                | "q" -> reply (sprintf " -> agent.CurrentQueueLength = %i" state.CurrentQueueLength)
                | "s" -> 
                    reply (" -> Server Status")                    
                    reply (sprintf "Server running since: %s" (appServer.StartedTime.ToString("yyyy-MMM-dd hh:mm:ss")))
                    reply (sprintf "Max Connection Limit = %i" appServer.Config.MaxConnectionNumber)
                    reply (sprintf "agent.CurrentQueueLength = %i" state.CurrentQueueLength)
                    reply (sprintf "Users   : %i" state.Users.Length)
                    reply (sprintf "Sessions: %i" appServer.SessionCount)                
                    reply (sprintf "Max Users   : = %i at %s" (fst appServer.MaxUserCount   )  ((snd appServer.MaxUserCount   ).ToString("yyyy-MM-dd HH:mm:ss:fff")))
                    reply (sprintf "Max Sessions: = %i at %s" (fst appServer.MaxSessionCount)  ((snd appServer.MaxSessionCount).ToString("yyyy-MM-dd HH:mm:ss:fff")))
                    reply (sprintf "MinScreenLogLevel = %A" appServer.MinScreenLogLevel)
                    reply (sprintf "MinFileLogLevel   = %A" appServer.MinFileLogLevel)
                    reply (sprintf "MinLogLevel       = %A" appServer.MinLogLevel)                    
                    reply (sprintf "SendBufferSize = %i"    appServer.Config.SendBufferSize)
                    reply (sprintf "SendingQueueSize = %i"  appServer.Config.SendingQueueSize)
                    reply (sprintf "Server Version = %A" (Reflection.Assembly.GetExecutingAssembly().GetName().Version))
                | "S" ->
                    let sessionIds = state.Users |> Seq.map (fun {Session = s} -> s)
                    let anonymous  = appServer.GetAllSessions() |> Seq.filter (fun s -> not (Seq.exists ((==) s.SessionID) sessionIds))
                    let lines      = anonymous |> Seq.map (fun s -> sprintf "Session: %s IP: %-15A Start: %A Last: %A" s.SessionID s.RemoteEndPoint.Address s.StartTime s.LastActiveTime)
                    let asText     = String.Join(nl, lines)
                    reply(sprintf " -> Sessions pending authentication %s%s" nl asText)
                | "c" -> 
                    let sessionIds = state.Users |> List.map (fun {Session = s} -> s)
                    let anonymous  = appServer.GetAllSessions() |> Seq.filter (fun s -> not (List.exists ((==) s.SessionID) sessionIds))
                    anonymous |> Seq.iter (fun s -> s.Close())
                    reply(sprintf " -> Clean up : %i sessions pending of authorization." (Seq.length anonymous))
                | "g" -> reply(" -> Garbage Collect"); GC.Collect()
                | "M" ->
                    let allMovies = "AllMovies"
                    let d = dict plist
                    match (d.["movies"], d.["ip"], d.["port"] , d.["delay"]) with
                    | (LList movieIds, LString ip, LInteger port, LInteger delay) ->
                        let mparams = movieIds |> Seq.choose (function (LString m) -> Some m | _ -> None)
                        let moveAll = mparams  |> Seq.exists ((==) allMovies)
                        let movies  = mparams  |> Seq.filter ((!=) allMovies)
                        let msg rcpts = writeMessage ("System.Movie.MoveToNetServer", rcpts, "MoveToNetServer", LPropList ["ip", LString ip; "port", LInteger port; "delay", LInteger delay] |> pickle valueP)
                        state.Users 
                            |> Seq.filter (fun {Movie = m} -> m != MvAdm && (moveAll <> Seq.exists ((==) m) movies))
                            |> Seq.iter (fun u -> send (msg [u.Name]) (appServer.GetSessionByID(u.Session)))
                    | _ -> log Warn "Admin Command: 'M' -> wrong format"
                | _ -> ()
            with exn -> reply(exn.ToString())

        let removeUser user users = List.filter (fun {Session = x} -> x != user.Session) users //to do handle user not there

        let appServer_NewRequestReceived (session:CustomProtocolSession) (requestInfo: Protocol.BinaryRequestInfo) =

            let disconnectUser sessionId = 
                let u = appServer.GetSessionByID(sessionId)
                if not (u = null) then u.Close()

            let antiFlood msg =
                if session.ProtocolVersion > (1,0) && (not (session.CheckNewMessage msg.errorCode)) then
                    log Warn "Message %i from session %s ip:%s is not valid: %A" msg.errorCode session.SessionID (ipAddress session) (prettyPrintMsg msg)
                    session.Close(CloseReason.ServerClosing)
            
            try 
                let msg = unpickle (messageU (if session.Authenticated then None else Some appServer.blowfishVector)) requestInfo.Body
                antiFlood msg
                if not (session.Authenticated) then
                    let (|Props|) lst p = List.choose (fun s -> (List.tryPick (function (str , LString v) when str = s -> Some v | _ -> None) p)) lst
                    match unpickle valueU msg.content with
                    | LList [LString moviename; LString userId; LString password]
                    | LPropList (Props ["movieID"; "userID"; "password"] [moviename; userId; password]) 
                        ->
                        if (appServer.MinLogLevel = Trace) then log Trace "Message is %A" (prettyPrintMsg msg)
                        let (protocol, client) =
                            match password.Split([|','|]) |> Array.collect (fun x -> x.Split([|'.'|])) |> Array.map Int32.TryParse with
                            | [|true,b1;true,b2;true,b3;true,b4|] -> ((b1, b2), (b3, b4))
                            | _                                   -> ((1 , 0 ), (2, 142))
                        session.ProtocolVersion <- protocol
                        session.ClientVersion   <- client

                        let Logon users = function
                            | Some sender -> log Fatal "User already logged in, session %s exists." sender.Session; users
                            | None -> 
                                let users = 
                                    match (List.tryFind (fun {Name = n; Movie = m} -> n == userId && m == moviename) users) with
                                    | None              -> users
                                    | Some existingUser ->                        
                                        disconnectUser existingUser.Session
                                        log Info "User already exists in that movie. Disconnect user:%s from movie:%s, session %s" userId moviename existingUser.Session
                                        removeUser existingUser users
                                send (writeMessage ("System", [userId], "Logon", pickle valueP (LString moviename))) session
                                session.Authenticated     <- true
                                appServer.MaxUserCount    <- Seq.maxBy fst [appServer.MaxUserCount   ; state.Users.Length, DateTime.UtcNow]
                                appServer.MaxSessionCount <- Seq.maxBy fst [appServer.MaxSessionCount; appServer.SessionCount, DateTime.UtcNow]
                                log Info "+ %-26s Connected to      Movie: %-22s from: %-15s using session:%s Protocol:%i.%i Client:%i.%i"
                                    userId moviename (ipAddress session) session.SessionID
                                    (fst session.ProtocolVersion) (snd session.ProtocolVersion)
                                    (fst session.ClientVersion  ) (snd session.ClientVersion)
                                {Name= userId; Movie= moviename; Groups= ["@AllUsers"]; Session= session.SessionID}::users                                
                        state.post session.SessionID (LogOnOff Logon)

                    | _ -> failwith "Extract login info failed, expecting a list or a property list."                        
                else
                    log Trace "Incoming Message"
                    if (appServer.MinLogLevel = Trace) then log Trace "Message is %A" (prettyPrintMsg msg)

                    let GetUsers (movie, group) _ = LPropList [("groupName", LString group) ; ("groupMembers", toLList (Seq.filter (fun {Movie = m; Groups = g} -> m == movie && Seq.exists ((==) group) g) state.Users |> Seq.map (fun {Name = n} -> n)))]
                    let GetUserCount movie      _ = LPropList [("movieID", LString movie) ; ("numberMembers", LInteger (Seq.filter (fun {Movie = m} -> m == movie) state.Users |> Seq.length))]
                    let GetMovies _ = toLList (Seq.map    (fun {Movie = m} -> m) state.Users |> Seq.distinctBy (fun (s:string) -> s.ToLowerInvariant()))
                    let GetGroupMembers group sender  = LPropList [("groupName", LString group) ; ("groupMembers", toLList (Seq.filter (fun {Movie = m; Groups = g} -> m == sender.Movie && List.exists ((==) group) g) state.Users |> Seq.map (fun {Name = n} -> n) |> Seq.sort))]

                    let Join group  users sender = {sender with Groups = group :: List.filter ((!=) group) sender.Groups} :: removeUser sender users
                    let Leave group users sender = {sender with Groups =          List.filter ((!=) group) sender.Groups} :: removeUser sender users
                    let Delete      users sender = disconnectUser sender.Session; removeUser sender users

                    let decodedRecipients = List.map (fun (r:string) ->
                        match r.Split([|'@'|]) with
                        | [|""  ; ""   |] -> None     , None
                        | [|""  ; movie|] -> None     , Some movie
                        | [|user; ""   |] -> Some user, None    
                        | [|user; movie|] -> Some user, Some movie       
                        | [|user|]        -> Some user, None       
                        | _               -> None     , None      )  msg.recipients
                    let serverMessage =
                        match (decodedRecipients                             , msg.subject         , lazy(unpickle valueU msg.content)) with
                        | [Some (CI "system.group.getUsers"    ), Some movie], _                   , Lazy(LString group) -> Function (GetUsers (movie, group))
                        | [Some (CI "system.movie.getUserCount"), Some movie], _                   , _                   -> Function (GetUserCount movie)
                        | [Some (CI "system.server.getMovies"  ), _         ], _                   , _                   -> Function  GetMovies
                        | [Some (CI "System"                   ), _         ], CI "getGroupMembers", Lazy(LString group) -> Function (GetGroupMembers group)
                        | [Some (CI "system.user.delete"       ), _         ], _                   , Lazy(LString user)  -> Command   Delete
                        | [Some (CI "System"                   ), None      ], CI "joinGroup"      , Lazy(LString group) -> Command  (Join group )
                        | [Some (CI "System"                   ), None      ], CI "leaveGroup"     , Lazy(LString group) -> Command  (Leave group)           
                        | [Some cmd                             , Some _    ], "adminCmd"          , Lazy(LPropList lst) -> AdminCmd (cmd, lst)
                        | [Some _                               , Some MvSQL], "DBexec"            , Lazy(LPropList lst) -> DBCmd lst
                        | recipients                                         , _                   , _                   -> UserMsg   recipients
                    match (serverMessage, lazy (Seq.tryFind (fun {Session = s} -> s == session.SessionID) state.Users)) with
                    | Function f, Lazy None -> log Debug "Session not found: %s , function: %A, message: %A" session.SessionID f (prettyPrintMsg msg)
                    | UserMsg _ , Lazy None -> log Debug "Session not found: %s, message: %s"                session.SessionID   (prettyPrintMsg msg)
                    | Function f, Lazy (Some sender) ->
                        log Trace "Processing function ..."
                        let (sender, rcpts, content) = (Seq.head msg.recipients, [sender.Name], f sender)                        
                        log Trace "Ready to send result of processing function."
                        let msg = writeMessage (sender, rcpts, msg.subject, pickle valueP content)
                        if (appServer.MinLogLevel = Trace) then log Trace "Sending result: %s" (prettyPrintMsg msg)                        
                        send msg session
                    | UserMsg rcpts, Lazy (Some sender) ->
                        log Trace "Processing message ..."
                        let users = state.Users
                        let sendMany recipients =
                            let (destUsers, destName, destMovie) =
                                match recipients with
                                | Some user, None       -> Seq.filter (fun {Movie = m; Name   = n} -> n == user && m == sender.Movie) users                      , sender.Name, None
                                | None     , Some group -> Seq.filter (fun {Movie = m; Groups = g} -> m == sender.Movie && Seq.exists ((==) ("@"+group)) g) users, sender.Name, None
                                | Some user, Some movie -> Seq.filter (fun {Movie = m; Name   = n} -> n == user && m == movie) users                             , sender.Name, if sender.Movie == movie then None else Some sender.Movie
                                | None     , None       -> Seq.empty, sender.Name, None
                            Seq.iter (fun user -> send {msg with errorCode = 0; timeStamp = 0; sender = destName + defaultArg (Option.map (fun x -> "@"+x) destMovie) ""} (appServer.GetSessionByID(user.Session))) destUsers
                        Seq.iter sendMany rcpts
                    | Command   c   , _ -> state.post session.SessionID (ServerCmd c)
                    | AdminCmd (c,l), _ -> processAdminCommand session c l
                    | DBCmd l, _ -> processDBCommand session l
                    | Invalid       , _ -> log Warn "Invalid message format received."

            with exn -> 
                log Warn "Process message failed for session: %s IP: %s error: %A" session.SessionID (ipAddress session) exn
                if not session.Authenticated then session.Close(CloseReason.ProtocolError)

        let login (session:CustomProtocolSession) =
            if isInBlackList session then
                log Warn "Session %s from %A is Blacklisted, will be disconnected." session.SessionID (ipAddress session)
                session.Close(CloseReason.ServerClosing)
            else log Debug "Accepted session: %s from: %s" session.SessionID (ipAddress session)

        let logout (session:CustomProtocolSession) (reason:CloseReason) =
            log Debug "Close Socket session. %s - reason: %s" session.SessionID (reason.ToString())
            let DetectDisconnect users = function
                | Some sender -> log Info "- %-26s Disconnected from Movie: %-22s from: %-15s using session:%s . Reason: %A" sender.Name sender.Movie (ipAddress session) sender.Session reason; removeUser sender users
                | None        -> log Info "? User: %s from %s disconnected before being logged in. Reason: %A" session.SessionID                      (ipAddress session)                reason; users
            state.post session.SessionID (LogOnOff DetectDisconnect)

        appServer.add_NewRequestReceived(new RequestHandler<CustomProtocolSession, Protocol.BinaryRequestInfo>(appServer_NewRequestReceived))
        appServer.add_NewSessionConnected(new SessionHandler<CustomProtocolSession>(login))
        appServer.add_SessionClosed(new SessionHandler<CustomProtocolSession,CloseReason>(logout))


        let status e =
            let users = state.Users
            let stats = 
                users 
                |> Seq.groupBy (fun {Movie = m} -> m.ToLowerInvariant()) 
                |> Seq.map (fun (m, u) -> (m, Seq.length u))

            match appServer.dbCnString, appServer.spStats with
            | Some s, Some sp ->
                try
                    use conn = new SqlConnection(s)
                    conn.Open()
                    use command = new SqlCommand(sp, conn)
                    command.CommandType <- Data.CommandType.StoredProcedure
                    let mv = command.Parameters.AddWithValue("@movie", "")
                    let uc = command.Parameters.AddWithValue("@usercount", 0)
                    stats 
                        |> Seq.iter (fun (m, u) ->
                            mv.Value <- m
                            uc.Value <- u
                            command.ExecuteNonQuery() |> ignore)
                with exn -> log Error "DB-Execution failed: %A" exn
            | _   -> ()
            let qs = state.CurrentQueueLength
            log (if qs > 1000 then Fatal elif qs > 100 then Error elif qs > 10 then Warn else Trace) "Queue size is %i" qs
            log Debug "Users   : %i" state.Users.Length
            log Debug "Sessions: %i" appServer.SessionCount
            appServer.GetAllSessions() |> Seq.iter (fun s -> 
                if (not s.Authenticated && (DateTime.Now - s.StartTime).TotalSeconds > 60.) then 
                    log Debug "Session %s from %s Timeout." s.SessionID (ipAddress s)
                    s.Close(CloseReason.TimeOut))
        let timer = new System.Timers.Timer(15000.)
        timer.Elapsed.Add(status)
        timer.Enabled <- true
                
        base.OnStarted()