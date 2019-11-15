module agent_SystemWorkTracker


type private agent_SystemWorkTrackerMsg = 
| Decrement
| Increment
| IsIdle of AsyncReplyChannel<bool>


type agent_SystemWorkTracker() = 

    let agent = 
        let mutable _tasks = 0u
        MailboxProcessor<agent_SystemWorkTrackerMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Decrement -> 
                            _tasks <- _tasks - 1u
                        | Increment -> 
                            _tasks <- _tasks + 1u
                        | IsIdle replyChannel ->
                            replyChannel.Reply(inbox.CurrentQueueLength = 0 && _tasks = 0u)
                }
            )
    member _.End = agent.Post Decrement
    member _.IsIdle = agent.PostAndReply IsIdle
    member _.Start = agent.Post Increment
    
    
       
