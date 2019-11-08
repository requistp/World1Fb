module MemoryManager
open agent_Memories


type MemoryManager() =
    let agentForMemories = new agent_Memories()
    
    member _.Init memories = agentForMemories.Init memories
    
    member _.Record memory = agentForMemories.Record memory

    

