module agent_EventListeners
open agent_GameLog
open CommonGenericFunctions
open EventTypes


type GameEventCallback = uint32 -> GameEventTypes -> Result<string option,string>


type private agentListenersMsg =
    | Execute of round:uint32 * gameEvent:GameEventTypes 
    | Register of listener:string * gameEventID:byte * GameEventCallback


type agent_EventListeners(log:agent_GameLog) =

    let agentListeners =
        let mutable _listeners = Map.empty:Map<byte,(string*GameEventCallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute (round,gameEvent) ->
                            match _listeners.ContainsKey gameEvent.GameEventID with
                            | false -> 
                                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i" " * " "<none>" (gameEvent.GameEventType()) gameEvent.EntityID)
                            | true ->
                                _listeners.Item gameEvent.GameEventID
                                |> Array.Parallel.iter (fun (listener,callback) -> 
                                    let result = callback round gameEvent
                                    let res_ToStrings =
                                        match result with
                                        | Error x -> ("Err", " : " + x)
                                        | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
                                    log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i%s" (fst res_ToStrings) listener (gameEvent.GameEventType()) gameEvent.EntityID (snd res_ToStrings)))
                        | Register (listener,eventType,callback) -> 
                            _listeners <- Map_AppendValueToArrayNonUnique _listeners eventType (listener,callback)
                            log.Log 0u (sprintf "%-3s | %-20s -> %-30s" "Ok " "Registered System" listener)
                }
            )
    member _.Execute round gameEvent = agentListeners.Post (Execute (round,gameEvent))
    member _.PendingUpdates = agentListeners.CurrentQueueLength > 0
    member _.Register listener eventTypeID callback = agentListeners.Post (Register (listener,eventTypeID,callback))

