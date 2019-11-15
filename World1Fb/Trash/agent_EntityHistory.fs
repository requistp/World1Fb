module agent_EntityHistory
//open Component
//open LocationTypes


//type historyTuple = Map<uint32,Component[]> * Map<byte,uint32[]> * Map<LocationDataInt,uint32[]>


//type private agent_HistoryMsg = 
//    | GetHistory of round:uint32 option * history:AsyncReplyChannel<historyTuple>
//    | GetAllHistory of allHistory:AsyncReplyChannel<Map<uint32,historyTuple> >
//    | InitHistory of allHistory:Map<uint32,historyTuple>
//    | RecordHistory of round:uint32 * history:historyTuple


//type agent_EntityHistory() =    
//    let agentHistory =
//        let mutable _history = Map.empty<uint32,historyTuple>
//        MailboxProcessor<agent_HistoryMsg>.Start(
//            fun inbox ->
//                async { 
//                    while true do
//                        let! msg = inbox.Receive()
//                        match msg with
//                        | GetHistory (round,replyChannel) ->
//                            replyChannel.Reply(
//                                match round with
//                                | Some r -> _history.Item r
//                                | None -> _history.Item (uint32 (_history.Count - 1))
//                                )
//                        | GetAllHistory replyChannel ->
//                            replyChannel.Reply(_history)
//                        | InitHistory hs ->
//                            _history <- hs
//                        | RecordHistory (round,h) -> 
//                            _history <- _history.Add(round,h)
//                }
//            )
//    member _.GetHistory (round:uint32 option) = agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
//    member _.GetAllHistory = agentHistory.PostAndReply GetAllHistory
//    member _.InitHistory allHistory = agentHistory.Post (InitHistory allHistory)
//    //member _.PendingUpdates = agentHistory.CurrentQueueLength > 0
//    member _.RecordHistory round history = agentHistory.Post (RecordHistory (round,history))

