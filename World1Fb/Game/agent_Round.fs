module agent_Round


type private agent_RoundMsg =
    | GetRound of AsyncReplyChannel<uint32>
    | IncrementRound
    | InitRound of round:uint32


type agent_Round() =

    let agentRound =
        let mutable _round = 0u
        MailboxProcessor<agent_RoundMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetRound replyChannel ->
                            replyChannel.Reply(_round)
                        | IncrementRound ->
                            _round <- _round + 1u
                        | InitRound round ->
                            _round <- round
                }
            )
    member _.Get() = agentRound.PostAndReply GetRound
    member _.Increment = agentRound.Post IncrementRound
    member _.Init round = agentRound.Post (InitRound round)


