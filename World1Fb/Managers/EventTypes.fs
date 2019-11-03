module EventTypes
open Component
open ComponentEnums


type Event_ActionEat = { EntityID:uint32 }
    with 
    static member ID = 1uy

type Event_ActionMovement = { EntityID:uint32; Direction:MovementDirection }
    with 
    static member ID = 2uy

type Event_CreateEntity = { EntityID:uint32;  Components:Component[] }
    with 
    static member ID = 3uy

type Event_Eaten = { EaterID:uint32; EateeID:uint32; Quantity:int }
    with 
    static member ID = 4uy

type Event_FoodAllEaten = { EaterID:uint32; EateeID:uint32 }
    with 
    static member ID = 5uy

type Event_KillAllEaten = { EaterID:uint32; EateeID:uint32 }
    with 
    static member ID = 6uy

type Event_Metabolize = { EntityID:uint32 }
    with 
    static member ID = 7uy

type Event_Movement = { EntityID:uint32; Direction:MovementDirection }
    with 
    static member ID = 8uy

type Event_PlantGrowth = { EntityID:uint32 }
    with 
    static member ID = 9uy

type Event_PlantReproduce = { EntityID:uint32 }
    with 
    static member ID = 10uy

type Event_ScheduleEvent = { Frequency:uint32 }
    with 
    static member ID = 11uy

type Event_Starving = { EntityID:uint32 }
    with 
    static member ID = 12uy

type GameEventTypes =
    | Action_Eat of Event_ActionEat
    | Action_Movement of Event_ActionMovement
    | CreateEntity of Event_CreateEntity
    | Eaten of Event_Eaten
    | Food_AllEaten of Event_FoodAllEaten
    | Kill_AllEaten of Event_KillAllEaten
    | Metabolize of Event_Metabolize
    | Movement of Event_Movement
    | PlantRegrowth of Event_PlantGrowth
    | PlantReproduce of Event_PlantReproduce
    | ScheduleEvent of Event_ScheduleEvent * GameEventTypes
    | Starving of Event_Starving
    member me.EntityID =
        match me with
        | Action_Eat d -> d.EntityID
        | Action_Movement d -> d.EntityID
        | CreateEntity d -> d.EntityID
        | Eaten d -> d.EaterID
        | Food_AllEaten d -> d.EateeID
        | Kill_AllEaten d -> d.EateeID
        | Metabolize d -> d.EntityID
        | Movement d -> d.EntityID
        | PlantRegrowth d -> d.EntityID
        | PlantReproduce d -> d.EntityID
        | ScheduleEvent (_,ge) -> ge.EntityID
        | Starving d -> d.EntityID
    member me.GameEventID = 
        match me with
        | Action_Eat _ -> Event_ActionEat.ID
        | Action_Movement _ -> Event_ActionMovement.ID
        | CreateEntity _ -> Event_CreateEntity.ID
        | Eaten _ -> Event_Eaten.ID
        | Food_AllEaten _ -> Event_FoodAllEaten.ID
        | Kill_AllEaten _ -> Event_KillAllEaten.ID
        | Metabolize _ -> Event_Metabolize.ID
        | Movement _ -> Event_Movement.ID
        | PlantRegrowth _ -> PlantGrowthComponent.ID
        | PlantReproduce _ -> Event_PlantReproduce.ID
        | ScheduleEvent _ -> Event_ScheduleEvent.ID
        | Starving _ -> Event_Starving.ID
    member me.GameEventType() = 
        match me with
        | Action_Eat _ -> "Action_Eat"
        | Action_Movement _ -> "Action_Movement"
        | CreateEntity _ -> "CreateEntity"
        | Eaten _ -> "Eaten"
        | Food_AllEaten _ -> "Food_AllEaten"
        | Kill_AllEaten _ -> "Kill_AllEaten"
        | Metabolize _ -> "Metabolize"
        | Movement _ -> "Movement"
        | PlantRegrowth _ -> "PlantRegrowth"
        | PlantReproduce _ -> "PlantReproduce"
        | ScheduleEvent _ -> "ScheduleEvent"
        | Starving _ -> "Starving"
    member me.ToActionEat = 
        let (Action_Eat d) = me
        d
    member me.ToAction_Movement = 
        let (Action_Movement d) = me
        d
    member me.ToCreateEntity = 
        let (CreateEntity d) = me
        d
    member me.ToEaten = 
        let (Eaten d) = me
        d
    member me.ToFoodAllEaten = 
        let (Food_AllEaten d) = me
        d
    member me.ToKillAllEaten = 
        let (Kill_AllEaten d) = me
        d
    member me.ToMetabolize = 
        let (Metabolize d) = me
        d
    member me.ToMovement = 
        let (Movement d) = me
        d
    member me.ToPlantRegrowth = 
        let (PlantRegrowth d) = me
        d
    member me.ToPlantReproduce = 
        let (PlantReproduce d) = me
        d
    member me.ToScheduleEvent = 
        let (ScheduleEvent (sed,ge)) = me
        (sed,ge)
    member me.ToStarving = 
        let (Starving d) = me
        d


