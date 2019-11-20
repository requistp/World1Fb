module agent_IDManager


type private agent_IDManagerMsg = 
    | GetID of lastID:AsyncReplyChannel<uint32>
    | InitID of maxID:uint32
    | NewID of newID:AsyncReplyChannel<uint32>


type agent_IDManager() =

    let agent_ID =
        let mutable _maxID = 0u        
        MailboxProcessor<agent_IDManagerMsg >.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetID replyChannel -> 
                            replyChannel.Reply(_maxID)
                        | InitID startID -> 
                            _maxID <- startID
                        | NewID replyChannel -> 
                            _maxID <- _maxID + 1u
                            replyChannel.Reply(_maxID)
                }
            )

    member _.GetMaxID() = agent_ID.PostAndReply GetID
    member _.GetNewID() = agent_ID.PostAndReply NewID
    member _.Init startID = agent_ID.Post (InitID startID)


