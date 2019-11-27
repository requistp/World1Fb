module TerrainComponent
open CommonGenericFunctions
open ComponentEnums
open System.Runtime.CompilerServices


//[<IsByRefLike; Struct>]
[<Struct>]
type TerrainComponent(id:ComponentID, eid:EntityID, terrain:TerrainType) = 
    member _.ID = id
    member _.EntityID = eid
    member _.Terrain = terrain

        
        