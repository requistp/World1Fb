module agent_EntityID


type private agent_EntityIDMsg = 
| GetMax of AsyncReplyChannel<uint32>
| GetNew of AsyncReplyChannel<uint32>
| Init of uint32


type agent_EntityID() = 
    let agent =
        let mutable _maxEntityID = 0u
        MailboxProcessor<agent_EntityIDMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetMax replyChannel -> 
                            replyChannel.Reply(_maxEntityID)
                        | GetNew replyChannel -> 
                            _maxEntityID <- _maxEntityID + 1u
                            replyChannel.Reply(_maxEntityID)
                        | Init startMax -> 
                            _maxEntityID <- startMax
                }
            )

    member this.GetMaxID = 
        agent.PostAndReply GetMax
    
    member this.GetNewID = 
        agent.PostAndReply GetNew
    
    member this.Init (startMax:uint32) = 
        agent.Post (Init startMax)

    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0


