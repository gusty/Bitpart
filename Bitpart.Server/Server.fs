namespace Bitpart.Server

open System
open System.Configuration
open System.Data.SqlClient
open System.Text.RegularExpressions
open FsControl.Operators
open SuperSocket.SocketBase
open SuperSocket.SocketBase.Protocol
open SuperSocket.Facility.Protocol
open Bitpart.Log
open Bitpart.Utils
open Bitpart.Lingo
open Bitpart.Lingo.Pickler
open Bitpart.Multiuser

module internal LogTemp = 
    let log level = logf "Bitpart.Server" level
    setLogFileName "Server-log"
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
            let! session, cmd = inbox.Receive()
            users <-
                match cmd, (tryFind (fun {Session = s} -> s == session) users) with
                | LogOnOff  f, sender      -> f users sender
                | ServerCmd f, Some sender -> f users sender
                | ServerCmd f, None        -> log Error "Session not found: %s." session; users
            return! (loop())}
        loop())
    do agent.Error.Add (log Fatal "Terminate execution due to unhandled exception in the StateMailboxProcessor. Exception was: %A")
    member this.Users              with get() = users
    member this.CurrentQueueLength with get() = agent.CurrentQueueLength
    member this.post sessionID mbxmsg = agent.Post (sessionID, mbxmsg)

type AntiFloodRule = {RuleId: string; Regex: Regex; Counter: Counter}

type AntiFloodConfig() =
    inherit ConfigurationElement()
    let mutable regex = null
    [<ConfigurationProperty("id"        , IsKey = true , IsRequired = true)>] member this.Id         with get() = base.["id"        ] :?> _ and set(value:string) = base.["id"        ] <- value
    [<ConfigurationProperty("subject"   , IsKey = false, IsRequired = true)>] member this.Subject    with get() = base.["subject"   ] :?> _ and set(value:string) = base.["subject"   ] <- value
    [<ConfigurationProperty("time"      , IsKey = false, IsRequired = true)>] member this.Time       with get() = base.["time"      ] :?> _ and set(value:float ) = base.["time"      ] <- value
    [<ConfigurationProperty("maxRepeats", IsKey = false, IsRequired = true)>] member this.MaxRepeats with get() = base.["maxRepeats"] :?> _ and set(value:int   ) = base.["maxRepeats"] <- value
    [<ConfigurationProperty("match"     , IsKey = false, IsRequired = true)>] member this.Match      with get() = base.["match"     ] :?> _ and set(value:float ) = base.["match"     ] <- value
    member this.CreateRule() = 
        match regex with null -> regex <- Regex(Regex.Escape(this.Subject).Replace(@"\*", ".*").Replace(@"\?", "."), RegexOptions.Singleline ||| RegexOptions.Compiled) | _ -> ()
        {RuleId = this.Id; Regex = regex; Counter = Counter(this.MaxRepeats, this.Time, this.Match)}

[<ConfigurationCollection(typeof<AntiFloodConfig>)>]
type AntiFloodConfigCollection() =
    inherit ConfigurationElementCollection()
    override this.CreateNewElement() = new AntiFloodConfig()  :> ConfigurationElement
    override this.GetElementKey(e:ConfigurationElement) = (e :?> AntiFloodConfig).Id :> obj

type AntiFlood() =
    let mutable brokenRule = None
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop() = async {
            let! time, rules, msg, cont, stop = inbox.Receive()
            match brokenRule with
            | None ->
                brokenRule <- rules |> tryFind (fun r -> r.Counter.Update(msg.content, time))
                match brokenRule with
                | None      -> cont()
                | Some rule -> stop rule
            | _ -> ()
            return! (loop())}
        loop())
    do agent.Error.Add (log Fatal "Terminate execution due to unhandled exception in the AntiFloodMailboxProcessor. Exception was: %A")
    member this.post time rules msg cont stop = agent.Post (time, rules, msg, cont, stop)

[<AllowNullLiteral>]
type CustomProtocolSession() =
    inherit AppSession<CustomProtocolSession, BinaryRequestInfo>()
    let messageLogSize = 100
    let lastMessages   = Array.zeroCreate messageLogSize
    member val AntiFloodState  = AntiFlood() with get
    member val AntiFloodRules  = []          with get, set
    member val Authenticated   = false       with get, set
    member val ProtocolVersion = 0, 0        with get, set
    member val ClientVersion   = 0, 0        with get, set
    member val MessageHash     = -1, null    with get, set
    member this.CheckMessageOrder msgId =
        let  v = lastMessages.[msgId % messageLogSize]
        if   v = msgId then log Warn "Duplicate message: %i - Session: %s" msgId this.SessionID; false
        elif v > msgId then log Warn "New messageId: %i is too old compared with previous one: %i - Session: %s" msgId v this.SessionID; false
        else lastMessages.[msgId % messageLogSize] <- msgId; true
    override this.HandleException(e) =
        log Fatal "Unhandled exception in session: %s : %A " this.SessionID e
        this.Close CloseReason.ApplicationError

type MyReceiveFilter (appServer,  appSession:CustomProtocolSession, remoteEndPoint) =
    inherit FixedHeaderReceiveFilter<BinaryRequestInfo> 6
    override this.GetBodyLengthFromHeader(header, offset, length) =
        if not (header.[offset] = 114uy) || not (header.[offset + 1] = 0uy) then
            log Warn "Invalid headers: %i - %i received from %A , session: %s . Client is %sauthenticated, will disconnect." header.[offset] header.[offset+1] remoteEndPoint appSession.SessionID (if appSession.Authenticated then "" else "not ")
            appSession.Close CloseReason.ProtocolError
            0
        else fromBytesWithOptions false (offset+2) header
    override this.ResolveRequestInfo(header, bodyBuffer, offset, length) = 
        BinaryRequestInfo (Text.Encoding.UTF8.GetString (header.Array, header.Offset, 2), SuperSocket.Common.BinaryUtil.CloneRange (bodyBuffer, offset, length))

type FilterFactory() =
    interface IReceiveFilterFactory<BinaryRequestInfo> with
        member this.CreateFilter (appServer,  appSession,  remoteEndPoint) =
            log Debug "New socket session: %s from %s" appSession.SessionID (appSession.RemoteEndPoint.Address.ToString())
            MyReceiveFilter (appServer, appSession :?> CustomProtocolSession,  remoteEndPoint) :> IReceiveFilter<BinaryRequestInfo>
   
type Protocol() = 
    inherit AppServer<CustomProtocolSession, BinaryRequestInfo> (FilterFactory())   
    
    let [<Literal>] MvAdm = "MovieAdmin"
    let [<Literal>] MvSQL = "MovieSQL"
    let assembly = Reflection.Assembly.GetExecutingAssembly()
    let path = IO.Path.GetDirectoryName assembly.Location

    let initialMinSLL, initialMinFLL = Info, Debug
    let state = State()
    member val BlackList         = [||]                            with get, set
    member val MaxUserCount      = 0, DateTime.MinValue            with get, set
    member val MaxSessionCount   = 0, DateTime.MinValue            with get, set
    member val MinScreenLogLevel = initialMinSLL                   with get, set
    member val MinFileLogLevel   = initialMinFLL                   with get, set
    member val MinLogLevel       = min initialMinSLL initialMinFLL with get, set
    member val encKey            = ""                              with get, set
    member val blowfishVector    = [||], [||], [||], [||], [||]    with get, set
    member val dbCnString        = None                            with get, set
    member val spStats           = None                            with get, set
    member val dbFnName          = "strFunction"                   with get, set
    member val dbFnArgs          = "lstArgs"                       with get, set
    member val dbFnResult        = "varResult"                     with get, set
    member val antiFloodRules    = [] : AntiFloodConfig list       with get, set

    override appServer.Setup (rootConfig, config) =
        appServer.encKey <- config.Options.["encryptionKey"]
        appServer.blowfishVector <- appServer.encKey |> encoding.GetBytes |> Bitpart.Blowfish.blowfishCypher
        let toOption (entry:string) = 
            let value = config.Options.[entry]
            if not (String.IsNullOrEmpty value) then Some value
            else log Info "No %s entry in config, this feature will be disabled." entry; None
            
        appServer.dbCnString     <- toOption "dbCnString"
        appServer.spStats        <- toOption "spStats"
        appServer.dbFnName       <- defaultArg (toOption "dbFnName")   appServer.dbFnName
        appServer.dbFnArgs       <- defaultArg (toOption "dbFnArgs")   appServer.dbFnArgs
        appServer.dbFnResult     <- defaultArg (toOption "dbFnResult") appServer.dbFnResult
        appServer.antiFloodRules <- config.GetChildConfig<AntiFloodConfigCollection> "antiflood" |> Seq.cast |> toList
        true
    
    override appServer.OnStartup() =
        setLogFileName "Server-log"
        let setScreenLogLevel level =
            appServer.MinScreenLogLevel <- level
            appServer.MinLogLevel       <- min appServer.MinScreenLogLevel appServer.MinFileLogLevel
            setScreenLogLevel level
        let setFileLogLevel level =
            appServer.MinFileLogLevel   <- level
            appServer.MinLogLevel       <- min appServer.MinScreenLogLevel appServer.MinFileLogLevel
            setFileLogLevel level
        setScreenLogLevel appServer.MinScreenLogLevel
        setFileLogLevel   appServer.MinFileLogLevel

        let isInBlackList (session:IAppSession) = exists ((=) (session.RemoteEndPoint.Address.ToString())) appServer.BlackList
        let ipAddress     (session:IAppSession) = session.RemoteEndPoint.Address.ToString()
        let isValid msg   (session:CustomProtocolSession) =
            let order, hashOK = 
                if session.ProtocolVersion > (1, 1) then
                    let bytes = toBytes msg.errorCode
                    let order, hashReceived = fromBytes (bytes.[..2] <|> [|0uy|]), bytes.[3]
                    let hashExpected order =
                        let d, r = divRem order 16
                        match session.MessageHash with
                        | i, k when i = d -> item r k
                        | _ ->
                            use md5 = Security.Cryptography.MD5CryptoServiceProvider.Create()
                            let h = md5.ComputeHash (encoding.GetBytes (appServer.encKey + session.SessionID + string d))
                            session.MessageHash <- d, h
                            h.[r]
                    order, hashReceived = hashExpected order
                else msg.errorCode, true
            if not hashOK || (session.ProtocolVersion > (1,0) && (not (session.CheckMessageOrder order))) then
                log Warn "Message %i from session %s ip:%s is not valid%s: %A" msg.errorCode session.SessionID (ipAddress session) (if hashOK then "" else ", hash doesn't match") (prettyPrintMsg msg)
                false
            else true

        let send message = function            
            | null -> log Debug "Session does not exists anymore."
            | (session:CustomProtocolSession) ->
                let bytes = packMessage None message
                if not (session.SocketSession.TrySend (ArraySegment bytes)) then
                    log Warn "Dropping Message to session: %s from %-15s , size was %-4i (sender = %s, rcpts = %A, subject = %s)." session.SessionID (ipAddress session) (length bytes) message.sender message.recipients message.subject

        let disconnectUser sessionId = match appServer.GetSessionByID sessionId with null -> () | u -> u.Close()
        let removeUser user users = filter (fun {Session = x} -> x != user.Session) users

        let GetUsers (movie, group) _ = LPropList [("groupName", LString group) ; ("groupMembers", toLList (filter (fun {Movie = m; Groups = g} -> m == movie && exists ((==) group) g) state.Users |>> fun {Name = n} -> n))]
        let GetUserCount movie      _ = LPropList [("movieID", LString movie) ; ("numberMembers", LInteger (filter (fun {Movie = m} -> m == movie) state.Users |> length))]
        let GetMovies _ = state.Users |>> (fun {Movie = m} -> m) |> distinctBy (fun (s:string) -> s.ToLowerInvariant()) |> toLList
        let GetGroupMembers group sender  = LPropList [("groupName", LString group) ; ("groupMembers", toLList (filter (fun {Movie = m; Groups = g} -> m == sender.Movie && exists ((==) group) g) state.Users |>> (fun {Name = n} -> n) |> sort))]

        let Join  group users sender = {sender with Groups = group :: filter ((!=) group) sender.Groups} :: removeUser sender users
        let Leave group users sender = {sender with Groups =          filter ((!=) group) sender.Groups} :: removeUser sender users
        let Delete      users sender = disconnectUser sender.Session                                      ; removeUser sender users

        let DetectDisconnect (session: CustomProtocolSession) (reason: CloseReason) users = function
            | Some sender -> log Info "- %-26s Disconnected from Movie: %-22s from: %-15s using session:%s . Reason: %A" sender.Name sender.Movie (ipAddress session) sender.Session reason; removeUser sender users
            | None        -> log Info "? User: %s from %s disconnected before being logged in. Reason: %A" session.SessionID                      (ipAddress session)                reason; users

        let Logon (session:CustomProtocolSession) userId movieId users = function
            | Some sender -> log Fatal "User already logged in, session %s exists." sender.Session; users
            | None -> 
                let users = 
                    match tryFind (fun {Name = n; Movie = m} -> n == userId && m == movieId) users with
                    | None              -> users
                    | Some existingUser ->                        
                        disconnectUser existingUser.Session
                        log Info "User already exists in that movie. Disconnect user:%s from movie:%s, session %s" userId movieId existingUser.Session
                        removeUser existingUser users
                send (writeMessage ("System", [userId], "Logon", pickle valueP (LString movieId))) session
                if session.ProtocolVersion > (1, 1) then send (writeMessage ("System", [userId], "SessionID", pickle valueP (LString session.SessionID))) session
                session.Authenticated     <- true
                appServer.MaxUserCount    <- maxBy fst [appServer.MaxUserCount   ; length state.Users    , DateTime.UtcNow]
                appServer.MaxSessionCount <- maxBy fst [appServer.MaxSessionCount; appServer.SessionCount, DateTime.UtcNow]
                log Info "+ %-26s Connected to      Movie: %-22s from: %-15s using session:%s Protocol:%i.%i Client:%i.%i" userId movieId (ipAddress session) session.SessionID <|| session.ProtocolVersion <|| session.ClientVersion
                {Name= userId; Movie= movieId; Groups= ["@AllUsers"]; Session= session.SessionID}::users


        let processDBCommand (session:CustomProtocolSession) plist =
            match appServer.dbCnString with
            | None   -> log Warn "execDbQuery received but no connection string specified: %A" plist
            | Some s ->
                let dct = dict plist
                match dct.TryGetValue appServer.dbFnName, dct.TryGetValue appServer.dbFnArgs with
                | (true, LString sp_name), (true, LList args) ->
                    log Trace "Msg Content is : %A" plist
                    let sp_args = String.concat "," (args |>> function 
                        | LString  e -> "'" + e.Replace ("'","''") + "'" 
                        | LInteger e -> "'" + string e + "'" 
                        | LVoid      -> "'" + "<Void>" + "'"
                        | s          -> failwith (sprintf "Unexpected DBExec message format received: %A" s))
                    log Trace "execDbQuery %s %s" sp_name sp_args
                    use conn = new SqlConnection (s)
                    conn.Open()
                    use command = new SqlCommand ("exec sp_" + sp_name + " " + sp_args, conn)
                    let lstResult = seq {
                        use reader = command.ExecuteReader()
                        while reader.Read() do
                            yield [0.. reader.FieldCount-1] |>> fun i -> 
                                if reader.GetFieldType i = typeof<Boolean> then if reader.GetBoolean i then "1" else "0" 
                                else reader.[i].ToString()} |> toList
                    log Trace "DB-Execution results: %A" lstResult
                    let result = toList plist @ [appServer.dbFnResult, lstResult |>> (map LString >> LList) |> LList]
                    log Trace "Result before sending to server: %A" (LPropList result)
                    session |> send (writeMessage ("System", state.Users |> filter (fun {Session = s} -> s == session.SessionID) |>> (fun {Name = n} -> n), "DBexec", LPropList result |> pickle valueP))
                | _ -> log Error "Wrong DBExec message content format: %A ." dct

        let processAdminCommand session cmd plist =
            let nl = Environment.NewLine
            let reply str = session |> send (writeMessage ("System", state.Users |> filter (fun {Session = s} -> s == session.SessionID) |>> (fun {Name = n} -> n), "Admin Command Reply", LString str |> pickle valueP))
            try
                match cmd with
                | "t" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Trace); setScreenLogLevel Trace
                | "d" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Debug); setScreenLogLevel Debug
                | "i" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Info ); setScreenLogLevel Info 
                | "w" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Warn ); setScreenLogLevel Warn 
                | "e" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Error); setScreenLogLevel Error
                | "f" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Fatal); setScreenLogLevel Fatal
                | "o" -> reply (sprintf " -> Changing minimum screen log Level to: %A" Off  ); setScreenLogLevel Off  
                | "T" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Trace); setFileLogLevel   Trace
                | "D" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Debug); setFileLogLevel   Debug
                | "I" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Info ); setFileLogLevel   Info 
                | "W" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Warn ); setFileLogLevel   Warn 
                | "E" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Error); setFileLogLevel   Error
                | "F" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Fatal); setFileLogLevel   Fatal
                | "O" -> reply (sprintf " -> Changing minimum file log Level to: %A"   Off  ); setFileLogLevel   Off  
                | "l" -> 
                    reply (sprintf " -> Loading and applying blacklist ...")
                    appServer.BlackList <- IO.File.ReadAllLines (path + @"\blacklist.txt")
                    appServer.GetAllSessions() |> filter isInBlackList |> map_ (fun s -> 
                        log Warn "Session %s from %s is Blacklisted, will be disconnected immediately." s.SessionID (ipAddress s)
                        s.Close CloseReason.ServerClosing)
                | "u" -> 
                    let users = state.Users
                    let detailed = users |> sort |>> fun u -> 
                        match appServer.GetSessionByID u.Session with
                        | null -> "Session no longer exists."
                        | s    -> sprintf "Name: %-26s , Movie: %-22s , Session: %s , IP:%-15s , Groups: %A" u.Name u.Movie u.Session (ipAddress s) u.Groups
                    let detail = String.concat nl detailed
                    reply (sprintf " -> User Details:%s%s%sUser Count: %i" nl detail nl (length users))
                | "m" ->
                    let users = state.Users
                    let detail = String.concat nl (users |> groupBy (fun {Movie = m} -> m.ToLowerInvariant()) |>> fun (m, u) -> sprintf "Movie: %-22s , Users:%3i" m (length u))
                    reply (sprintf " -> Movie Details:%s%s%sUser Count: %i" nl detail nl (length users))
                | "q" -> reply (sprintf " -> agent.CurrentQueueLength = %i" state.CurrentQueueLength)
                | "s" -> 
                    reply (" -> Server Status")                    
                    reply (sprintf "Server running since: %s" (appServer.StartedTime.ToString("yyyy-MMM-dd hh:mm:ss")))
                    reply (sprintf "Max Connection Limit = %i" appServer.Config.MaxConnectionNumber)
                    reply (sprintf "agent.CurrentQueueLength = %i" state.CurrentQueueLength)
                    reply (sprintf "Users   : %i" (length state.Users))
                    reply (sprintf "Sessions: %i" appServer.SessionCount)                
                    reply (sprintf "Max Users   : = %i at %s" (fst appServer.MaxUserCount   ) ((snd appServer.MaxUserCount   ).ToString("yyyy-MM-dd HH:mm:ss:fff")))
                    reply (sprintf "Max Sessions: = %i at %s" (fst appServer.MaxSessionCount) ((snd appServer.MaxSessionCount).ToString("yyyy-MM-dd HH:mm:ss:fff")))
                    reply (sprintf "MinScreenLogLevel = %A" appServer.MinScreenLogLevel)
                    reply (sprintf "MinFileLogLevel   = %A" appServer.MinFileLogLevel)
                    reply (sprintf "MinLogLevel       = %A" appServer.MinLogLevel)                    
                    reply (sprintf "SendBufferSize = %i"    appServer.Config.SendBufferSize)
                    reply (sprintf "SendingQueueSize = %i"  appServer.Config.SendingQueueSize)
                    reply (sprintf "Server Version = %A" (assembly.GetName().Version))
                    reply (sprintf "Path = %s" path)
                | "S" ->
                    let sessionIds = state.Users |>> fun {Session = s} -> s
                    let anonymous  = appServer.GetAllSessions() |> filter (fun s -> not (exists ((==) s.SessionID) sessionIds))
                    let lines      = anonymous |>> fun s -> sprintf "Session: %s IP: %-15A Start: %A Last: %A" s.SessionID s.RemoteEndPoint.Address s.StartTime s.LastActiveTime
                    let asText     = String.concat nl lines
                    reply (sprintf " -> Sessions pending authentication %s%s" nl asText)
                | "c" -> 
                    let sessionIds = state.Users |>> fun {Session = s} -> s
                    let anonymous  = appServer.GetAllSessions() |> filter (fun s -> not (exists ((==) s.SessionID) sessionIds))
                    anonymous |> map_ (fun s -> s.Close())
                    reply(sprintf " -> Clean up : %i sessions pending of authorization." (length anonymous))
                | "g" -> reply (" -> Garbage Collect"); GC.Collect()
                | "M" ->
                    let allMovies = "AllMovies"
                    let d = dict plist
                    match d.["movies"], d.["ip"], d.["port"] , d.["delay"] with
                    | LList movieIds, LString ip, LInteger port, LInteger delay ->
                        let mparams = movieIds |> choose (function (LString m) -> Some m | _ -> None)
                        let moveAll = mparams  |> exists ((==) allMovies)
                        let movies  = mparams  |> filter ((!=) allMovies)
                        let msg rcpts = writeMessage ("System.Movie.MoveToNetServer", rcpts, "MoveToNetServer", LPropList ["ip", LString ip; "port", LInteger port; "delay", LInteger delay] |> pickle valueP)
                        state.Users |> iter (fun u ->  if u.Movie != MvAdm && (moveAll <> exists ((==) u.Movie) movies) then send (msg [u.Name]) (appServer.GetSessionByID u.Session))
                    | _ -> log Warn "Admin Command: 'M' -> wrong format"
                | _ -> ()
            with exn -> reply (exn.ToString())


        let evtRequest (session :CustomProtocolSession) (requestInfo :Protocol.BinaryRequestInfo) =  
            try 
                let msg = unpickle (messageU (if session.Authenticated then None else Some appServer.blowfishVector)) requestInfo.Body
                if not (isValid msg session) then session.Close CloseReason.ServerClosing
                elif not session.Authenticated then
                    session.AntiFloodRules <- appServer.antiFloodRules |>> fun c -> c.CreateRule()
                    let (|Props|) lst p = choose (fun s -> tryPick (function (str, LString v) when str = s -> Some v | _ -> None) p) lst
                    match unpickle valueU msg.content with
                    | LList [LString movieId; LString userId; LString password]
                    | LPropList (Props ["movieID"; "userID"; "password"] [movieId; userId; password]) 
                        ->
                        if appServer.MinLogLevel = Trace then log Trace "Message is %A" (prettyPrintMsg msg)
                        let protocol, client =
                            match password.Split [|','|] >>= (fun x -> x.Split [|'.'|]) |>> (tryParse :_ -> int option) |> sequenceA with
                            | Some [|a;b;c;d|] -> (a, b), (c, d  )
                            | _                -> (1, 0), (2, 142)
                        session.ProtocolVersion <- protocol
                        session.ClientVersion   <- client 
                        state.post session.SessionID (LogOnOff (Logon session userId movieId))
                    | _ -> failwith "Extract login info failed, expecting a list or a property list."                        
                else
                    if appServer.MinLogLevel = Trace then log Trace "Incoming Message: %A" (prettyPrintMsg msg)
                    let recipients = msg.recipients |>> fun recipient -> (recipient+"@").Split [|'@'|] |>> (function "" -> None | s -> Some s) |> Seq.pairwise |> head
                    let serverMessage =
                        match recipients                                     , msg.subject         , lazy (unpickle valueU msg.content) with
                        | [Some (CI "system.group.getUsers"    ), Some movie], _                   , Lazy (LString group) -> Function (GetUsers (movie, group))
                        | [Some (CI "system.movie.getUserCount"), Some movie], _                   , _                    -> Function (GetUserCount movie)
                        | [Some (CI "system.server.getMovies"  ), _         ], _                   , _                    -> Function  GetMovies
                        | [Some (CI "System"                   ), _         ], CI "getGroupMembers", Lazy (LString group) -> Function (GetGroupMembers group)
                        | [Some (CI "system.user.delete"       ), _         ], _                   , Lazy (LString user)  -> Command   Delete
                        | [Some (CI "System"                   ), None      ], CI "joinGroup"      , Lazy (LString group) -> Command  (Join group )
                        | [Some (CI "System"                   ), None      ], CI "leaveGroup"     , Lazy (LString group) -> Command  (Leave group)           
                        | [Some cmd                             , Some _    ], "adminCmd"          , Lazy (LPropList lst) -> AdminCmd (cmd, lst)
                        | [Some _                               , Some MvSQL], "DBexec"            , Lazy (LPropList lst) -> DBCmd     lst
                        | _                                                                                               -> UserMsg   recipients
                    let processServerMessage() =
                        match serverMessage, lazy (tryFind (fun {Session = s} -> s == session.SessionID) state.Users) with
                        | Function f, Lazy None -> log Debug "Session not found: %s , function: %A, message: %A" session.SessionID f (prettyPrintMsg msg)
                        | UserMsg _ , Lazy None -> log Debug "Session not found: %s, message: %s"                session.SessionID   (prettyPrintMsg msg)
                        | Function f, Lazy (Some sender) ->
                            let sender, rcpts, content = head msg.recipients, [sender.Name], f sender
                            let msg = writeMessage (sender, rcpts, msg.subject, pickle valueP content)
                            if appServer.MinLogLevel = Trace then log Trace "Sending result: %s" (prettyPrintMsg msg)                        
                            send msg session
                        | UserMsg rcpts, Lazy (Some sender) -> rcpts |> map_ (fun recipient ->
                            let destUsers, destMovie =
                                match recipient with
                                | Some user, None       -> filter (fun {Movie = m; Name   = n} -> n == user && m == sender.Movie) state.Users                  , None
                                | None     , Some group -> filter (fun {Movie = m; Groups = g} -> m == sender.Movie && exists ((==) ("@"+group)) g) state.Users, None
                                | Some user, Some movie -> filter (fun {Movie = m; Name   = n} -> n == user && m == movie) state.Users                         , filter ((!=) movie) (Some sender.Movie)
                                | None     , None       -> mempty(), None
                            map_ (fun user -> send {msg with errorCode = 0; timeStamp = 0; sender = sender.Name + defaultArg (map ((+) "@") destMovie) ""} (appServer.GetSessionByID user.Session)) destUsers)     
                        | Command   cmd        , _ -> state.post session.SessionID (ServerCmd cmd)
                        | AdminCmd (cmd, plist), _ -> processAdminCommand session cmd plist
                        | DBCmd          plist , _ -> processDBCommand    session     plist
                        | Invalid              , _ -> log Warn "Invalid message format received."

                    match filter (fun {Regex = r} -> r.IsMatch msg.subject) session.AntiFloodRules with
                    | []    -> processServerMessage()
                    | rules -> session.AntiFloodState.post DateTime.Now rules msg processServerMessage (fun rule -> 
                                    log Warn "Messages %i from session %s ip:%s over rule '%s' tolerance, last message was: %A" msg.errorCode session.SessionID (ipAddress session) rule.RuleId (prettyPrintMsg msg)
                                    state.post session.SessionID (ServerCmd Delete))        
            with exn -> 
                log Warn "Process message failed for session: %s IP: %s error: %A" session.SessionID (ipAddress session) exn
                session.Close CloseReason.ProtocolError

        let evtLogin (session: CustomProtocolSession) =
            if isInBlackList session then
                log Warn "Session %s from %A is Blacklisted, will be disconnected." session.SessionID (ipAddress session)
                session.Close CloseReason.ServerClosing
            else log Debug "Accepted session: %s from: %s" session.SessionID (ipAddress session)

        let evtLogout (session: CustomProtocolSession) (reason: CloseReason) =
            log Debug "Close Socket session. %s - reason: %s" session.SessionID (reason.ToString())
            state.post session.SessionID (LogOnOff (DetectDisconnect session reason))

        let evtStatus _ = 
            (fun s sp ->
                try
                    use conn = new SqlConnection(s)
                    conn.Open()
                    use command = new SqlCommand (sp, conn)
                    command.CommandType <- Data.CommandType.StoredProcedure
                    let mv = command.Parameters.AddWithValue ("@movie", "")
                    let uc = command.Parameters.AddWithValue ("@usercount", 0)
                    state.Users |> groupBy (fun {Movie = m} -> m.ToLowerInvariant()) |>> map length |> map_ (fun (m, u) ->
                                mv.Value <- m
                                uc.Value <- u
                                command.ExecuteNonQuery() |> ignore)
                with exn -> log Error "DB-Execution failed: %A" exn) <!> appServer.dbCnString <*> appServer.spStats |> ignore
            let qs = state.CurrentQueueLength
            log (if qs > 1000 then Fatal elif qs > 100 then Error elif qs > 10 then Warn else Trace) "Queue size is %i" qs
            log Debug "Users   : %i" (length state.Users)
            log Debug "Sessions: %i" appServer.SessionCount
            appServer.GetAllSessions() |> map_ (fun s -> 
                if not s.Authenticated && (DateTime.Now - s.StartTime).TotalSeconds > 60. then 
                    log Debug "Session %s from %s Timeout." s.SessionID (ipAddress s)
                    s.Close CloseReason.TimeOut)

        appServer.add_NewRequestReceived  (RequestHandler<_, _> evtRequest)
        appServer.add_NewSessionConnected (SessionHandler<_>    evtLogin  )
        appServer.add_SessionClosed       (SessionHandler<_, _> evtLogout )
        let timer = new Timers.Timer 15000.
        timer.Elapsed.Add evtStatus
        timer.Enabled <- true
                
        base.OnStarted()