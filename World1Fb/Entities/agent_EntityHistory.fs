module agent_EntityHistory
open Component
open LocationTypes


type historyTuple = (Map<uint32,Component[]> * Map<byte,uint32[]> * Map<LocationDataInt,uint32[]>)


type private agent_EntityHistoryMsg = 
| Add of round:uint32 * historyTuple
| Init of Map<uint32,historyTuple>


type agent_EntityHistory() =

    let mutable _history = Map.empty<uint32,historyTuple>

    let agent =
        MailboxProcessor<agent_EntityHistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,h) -> 
                            _history <- _history.Add(round,h)
                        | Init hs ->
                            _history <- hs
                }
            )

    member _.Add round h = agent.Post (Add (round,h))

    member _.Get (round:uint32) = _history.Item round

    member _.GetCurrent = _history.Item (uint32 (_history.Count-1))

    member _.Init hs = agent.Post (Init hs)

    member _.PendingUpdates = agent.CurrentQueueLength > 0
