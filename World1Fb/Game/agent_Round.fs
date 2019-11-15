module agent_Round


type private agentRoundMsg =
| Get of AsyncReplyChannel<uint32>
| Increment
| Init of round:uint32


type agent_Round() =

    let agent =
        let mutable _round = 0u
        MailboxProcessor<agentRoundMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Get replyChannel ->
                            replyChannel.Reply(_round)
                        | Increment ->
                            _round <- _round + 1u
                        | Init round ->
                            _round <- round
                }
            )
    member _.Get() = agent.PostAndReply Get
    member _.Increment = agent.Post Increment
    member _.Init round = agent.Post (Init round)
    member _.PendingUpdates = agent.CurrentQueueLength > 0


