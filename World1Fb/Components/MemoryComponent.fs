module MemoryComponent
open LocationTypes


type MemoryTypes =
    | Sight of entityID:uint32
    | Sound of entityID:uint32



type MemoryComponent = 
    { 
        EntityID : uint32
        Memories : Map<LocationDataInt, uint32 * MemoryTypes[]> // round * entityIDs
        Retention : uint32
    } 

