module agent_EventListeners
open agent_GameEventLog
open CommonGenericFunctions
open EventTypes


type private agentListenersMsg =
    | Execute of uint32 * GameEventTypes 
    | Register of uint32 * string * byte * GameEventCallback


type agent_EventListeners(agentForLog:agent_GameEventLog) =

    let agent =
        let mutable _listeners = Map.empty:Map<byte,(string*GameEventCallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute (round,ge) ->
                            match _listeners.ContainsKey ge.GameEventID with
                            | false -> 
                                agentForLog.Log_NoListeners round ge
                            | true ->
                                _listeners.Item ge.GameEventID
                                |> Array.Parallel.iter (fun (listener,callback) -> agentForLog.Log_CallbackResult round (listener,callback,ge,callback ge))
                        | Register (round,listener,eventType,callback) -> 
                            _listeners <- Map_AppendValueToArrayNonUnique _listeners eventType (listener,callback)
                            agentForLog.Log_ListenerRegistered round listener
                }
            )

    member _.Execute round ge =
        agent.Post (Execute (round,ge))
    
    member _.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member _.Register round listener eventTypeID callback =
        agent.Post (Register (round,listener,eventTypeID,callback))

