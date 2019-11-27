module MovementComponent
open CommonGenericFunctions
open ComponentEnums

[<Struct>]
type MovementComponent(id:ComponentID, eid:EntityID, movesPerTurn:int) = 
    member _.ID = id
    member _.EntityID = eid
    member _.MovesPerTurn = movesPerTurn

