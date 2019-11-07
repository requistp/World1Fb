module agent_GameEventLog
open EventTypes
open InterruptTypes
open Logging

type GameEventCallback = GameEventTypes -> Result<string option,string>

type private agentLogTypes =
    | CallbackResult of listener:string * callback:GameEventCallback * gameEvent:GameEventTypes * result:Result<string option,string>
    | EndOfRoundCancelled of step:int * reason:string
    | InterruptResult of listener:string * callback:InterruptCall * interrupt:Interrupts * result:bool
    | InterruptRegistered of listener:string
    | ListenerRegistered of listener:string
    | NoEventListeners of GameEventTypes
    | NoInterruptListeners of Interrupts
    | ScheduledEvent of GameEventTypes
    override this.ToString() = 
        match this with
        | CallbackResult (listener,_,ge,result) -> 
            let res_ToStrings =
                match result with
                | Error x -> ("Err", " : " + x)
                | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
            sprintf "%-3s | %-20s -> %-30s #%7i%s" (fst res_ToStrings) listener (ge.GameEventType()) ge.EntityID (snd res_ToStrings)
        | EndOfRoundCancelled (step,reason) ->
            sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" step reason
        | InterruptResult (listener,_,interrupt,result) -> 
            sprintf "%-3s | %-20s -> %-30s #%7i" (if result then "Yes" else "no") listener (interrupt.ToString()) interrupt.EntityID 
        | InterruptRegistered s -> 
            sprintf "%-3s | %-20s -> %-30s" "Ok " "Interrupt Registered" s
        | ListenerRegistered s -> 
            sprintf "%-3s | %-20s -> %-30s" "Ok " "System Registered" s
        | NoEventListeners ge -> 
            sprintf "%-3s | %-20s -> %-30s #%7i" " * " "<none>" (ge.GameEventType()) ge.EntityID
        | NoInterruptListeners interrupt -> 
            sprintf "%-3s | %-20s -> %-30s #%7i" " * " "<none>" (interrupt.ToString()) interrupt.EntityID
        | ScheduledEvent se ->
            let sed,ge = se.ToScheduleEvent
            sprintf "%-3s | %-20s -> %-30s #%7i : Frequency:%i" "-->" "Scheduled Event" (ge.GameEventType()) ge.EntityID sed.Frequency

type private agentLogMsg =
    | Log of uint32 * agentLogTypes
    | SetLogging of bool
    | WriteLog

type agent_GameEventLog() =
    let agent =
        let mutable _log = Array.empty<uint32*agentLogTypes>
        let mutable _logging = true
        MailboxProcessor<agentLogMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Log (round,logdata) -> 
                            if (_logging) then 
                                _log <- [| round,logdata |] |> Array.append _log
                        | SetLogging b ->
                            _logging <- b
                        | WriteLog ->
                            if (_logging) then 
                                _log |> Array.iter (fun (r,lt) -> writeLog (sprintf "%7i | %s" r (lt.ToString())))
                                _log <- Array.empty
                }
            )

    member _.Log_CallbackResult round (listener,callback,gameEvent,result) = agent.Post (Log (round, CallbackResult (listener,callback,gameEvent,result)))

    member _.Log_EndOfRoundCancelled round step reason = agent.Post (Log (round, EndOfRoundCancelled (step,reason)))
   
    member _.Log_NoEventListeners round ge = agent.Post (Log (round, NoEventListeners ge))

    member _.Log_NoInterruptListeners round interrupt = agent.Post (Log (round, NoInterruptListeners interrupt))

    member _.Log_InterruptResult round (listener,callback,interrupt,result) = agent.Post (Log (round, InterruptResult (listener,callback,interrupt,result)))

    member _.Log_InterruptRegistered round listener = agent.Post (Log (round, InterruptRegistered listener))

    member _.Log_ListenerRegistered round listener = agent.Post (Log (round, ListenerRegistered listener))

    member _.Log_ScheduledEvent round se = agent.Post (Log (round, ScheduledEvent se))
    
    member _.PendingUpdates = agent.CurrentQueueLength > 0

    member _.SetLogging b = agent.Post (SetLogging b)

    member _.WriteLog = agent.Post WriteLog

