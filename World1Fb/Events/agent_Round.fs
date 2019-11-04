module agent_Round


type agentRoundMsg =
    | Get of AsyncReplyChannel<uint32>
    | Increment


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
                }
            )

    member _.Get = agent.PostAndReply Get

    member _.Increment = agent.Post Increment

    member _.PendingUpdates = 
        agent.CurrentQueueLength > 0

