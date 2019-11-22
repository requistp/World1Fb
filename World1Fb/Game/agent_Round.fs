module agent_Round
open CommonGenericFunctions


type private agent_RoundMsg =
    | GetRound of AsyncReplyChannel<RoundNumber>
    | IncrementRound of AsyncReplyChannel<RoundNumber>
    | InitRound of RoundNumber


type agent_Round() =

    let agentRound =
        let mutable _round = RoundNumber(0u)
        MailboxProcessor<agent_RoundMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetRound replyChannel ->
                            replyChannel.Reply(_round)
                        | IncrementRound replyChannel ->
                            _round <- _round + 1u
                            replyChannel.Reply(_round)
                        | InitRound round ->
                            _round <- round
                }
            )
    member _.Get() = agentRound.PostAndReply GetRound
    member _.Increment = agentRound.PostAndReply IncrementRound
    member _.Init round = agentRound.Post (InitRound round)


