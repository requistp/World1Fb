module agent_Memories
open Component
open LocationTypes

// ExperienceMemory -- maybe put Gave Events here
type Memory = { EntityID:uint32; Time:uint32; Location:LocationDataInt; OtherEntityID:uint32 }


type MemoryTypes =
    | Sight of Memory
    | Sound of Memory


type agentMemoryMsg =
//| Get of AsyncReplyChannel<uint32>
| Record of MemoryTypes
| Init of MemoryTypes[]


type agent_Memories() =

    let agent =
        let mutable _memories = Array.empty<MemoryTypes>
        MailboxProcessor<agentMemoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        //| Get replyChannel ->
                        //    replyChannel.Reply(_round)
                        | Record memory ->
                            _memories <- [|memory|] |> Array.append _memories
                        | Init memories ->
                            _memories <- memories
                }
            )

    //member _.Get = agent.PostAndReply Get

    member _.Record memory = agent.Post (Record memory)

    member _.Init memories = agent.Post (Init memories)

    member _.PendingUpdates = agent.CurrentQueueLength > 0
