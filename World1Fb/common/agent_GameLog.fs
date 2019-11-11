module agent_GameLog
open Logging


type private agentLogMsg =
| Log of uint32 * string // Typical Format: "%-3s | %-20s -> %-30s #%7i%s"
| SetLogging of bool
| WriteLog


type agent_GameLog() =
    let agent =
        let mutable _log = Array.empty<uint32*string>
        let mutable _logging = true
        MailboxProcessor<agentLogMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Log (round,s) -> 
                            if (_logging) then 
                                _log <- Array.append _log [|round,s|]
                        | SetLogging b ->
                            _logging <- b
                        | WriteLog ->
                            if (_logging) then 
                                _log |> Array.iter (fun (r,s) -> writeLog (sprintf "%7i | %s" r s))
                                _log <- Array.empty
                }
            )
    
    member _.Log round s = agent.Post (Log (round,s))

    member _.SetLogging b = agent.Post (SetLogging b)

    member _.WriteLog = agent.Post WriteLog

