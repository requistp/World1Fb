module MovementComponent
open CommonGenericFunctions
open ComponentEnums


type MovementComponent = 
    {
        ID : ComponentID
        EntityID : EntityID
        MovesPerTurn : int 
    }

