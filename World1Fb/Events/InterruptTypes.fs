module InterruptTypes


let Interrupt_CanMateID = 1uy


type Interrupts = 
    | CanMate of entityID:uint32
    member me.EntityID = 
        match me with
        | CanMate eid -> eid
    member me.ID = 
        match me with
        | CanMate _ -> Interrupt_CanMateID
    override me.ToString() =
        match me with
        | CanMate _ -> "CanMate"


type InterruptCall = Interrupts -> bool