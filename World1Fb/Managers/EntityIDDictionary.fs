module EntityIDDictionary


type EntityIDDictionaryMsg = 
    | Max of AsyncReplyChannel<uint32>
    | New

type EntityIDDictionary() =
    let listAgent =
        let mutable _maxEntityID = 0u

        let add =
            _maxEntityID <- _maxEntityID + 1u

        MailboxProcessor<EntityIDDictionaryMsg>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! msg = inbox.Receive()
                            match msg with
                            | New -> add
                            | Max replyChannel -> replyChannel.Reply(_maxEntityID)
                    }
            )
    
    member this.New = listAgent.Post New

    member this.Max = listAgent.PostAndReply Max


