module agent_EntityHistory
open Component
open LocationTypes


type historyTuple = Map<uint32,Component[]> * Map<byte,uint32[]> * Map<LocationDataInt,uint32[]>


type private agent_EntityHistoryMsg = 
| Add of round:uint32 * historyTuple
| Get of round:uint32 option * AsyncReplyChannel<historyTuple>
| GetAll of AsyncReplyChannel<Map<uint32,historyTuple> >
| Init of Map<uint32,historyTuple>


type agent_EntityHistory() =    
    let agent =
        let mutable _history = Map.empty<uint32,historyTuple>
        MailboxProcessor<agent_EntityHistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,h) -> 
                            _history <- _history.Add(round,h)
                        | Get (round,replyChannel) ->
                            replyChannel.Reply(
                                match round with
                                | Some r -> _history.Item r
                                | None -> _history.Item (uint32 (_history.Count - 1))
                                )
                        | GetAll replyChannel ->
                            replyChannel.Reply(_history)
                        | Init hs ->
                            _history <- hs
                }
            )

    member _.Add round h = agent.Post (Add (round,h))

    member _.Get (round:uint32 option) = agent.PostAndReply (fun replyChannel -> Get (round,replyChannel))

    member _.GetAll = agent.PostAndReply GetAll

    member _.Init hs = agent.Post (Init hs)

    member _.PendingUpdates = agent.CurrentQueueLength > 0
