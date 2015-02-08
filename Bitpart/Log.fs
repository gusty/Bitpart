namespace Bitpart

open System

type LogLevel = Trace = 0 | Debug = 1 | Info = 2 | Warn = 3 | Error = 4 | Fatal = 5 | Off = 6
   
module Log =

    let Trace, Debug, Info, Warn, Error, Fatal, Off = 
        LogLevel.Trace, LogLevel.Debug, LogLevel.Info, LogLevel.Warn, LogLevel.Error, LogLevel.Fatal, LogLevel.Off

    open System

    let minScreenLogLevel = Info
    let minFileLogLevel   = Trace
    let defaultFileName   = "log"
    let path = IO.Path.Combine(IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location), "Logs")

    let colLevel = function
        | LogLevel.Trace
        | LogLevel.Debug -> ConsoleColor.DarkGray
        | LogLevel.Info  -> ConsoleColor.Gray  
        | LogLevel.Warn  -> ConsoleColor.Yellow
        | _              -> ConsoleColor.Red    

    let logLine appmodule level (time:DateTime) str minScreenLogLevel minFileLogLevel fileName =
        let printToConsole level (text:string) =
            let originalColor = Console.ForegroundColor
            Console.ForegroundColor <- colLevel level
            Console.WriteLine text
            Console.ForegroundColor <- originalColor

        let formatLine appmodule = sprintf "%s| %s| %A| %s" (time.ToString("yyyy-MM-dd HH:mm:ss:fff")) appmodule
        if level >= minScreenLogLevel then printToConsole level (formatLine appmodule level str)
        if level >= minFileLogLevel then 
            try IO.File.AppendAllLines (path + "\\" + fileName + "-" + DateTime.Now.ToString("yyyyMMdd") + ".csv", [formatLine appmodule level str])
            with exn -> printToConsole Error (formatLine "Logging" Error (sprintf "Could not log into file %s : %A" fileName exn))

    type LogMessage =
        | Log of DateTime * string * LogLevel * string
        | SetScreenLevel of LogLevel
        | SetFileLevel   of LogLevel
        | SetFileName    of string   

    let logAgent = MailboxProcessor.Start (fun inbox ->
        let rec loop sl fl fn = async { 
            let! msg = inbox.Receive()
            let (sl, fl, fn) =
                try                        
                    match msg with
                    | Log (time, appmodule, level, str) -> 
                        logLine appmodule level time str sl fl fn
                        sl, fl, fn
                    | SetScreenLevel level -> level, fl   , fn
                    | SetFileLevel   level -> sl   , level, fn
                    | SetFileName    name  -> sl   , fl   , name                    
                with exn -> sl, fl, fn
            return! loop sl fl fn }
        loop minScreenLogLevel minFileLogLevel defaultFileName)

    let log appmodule level str = logAgent.Post (Log (DateTime.UtcNow, appmodule, level, str))

    let setScreenLogLevel level = logAgent.Post (SetScreenLevel level)
    let setFileLogLevel   level = logAgent.Post (SetFileLevel   level)
    let setLogFileName    name  = logAgent.Post (SetFileName    name )

    let logf appmodule level str = Printf.kprintf (log appmodule level) str