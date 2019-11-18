module EventTypes
open Component
open ComponentEnums

let Event_ActionEat_ID = 1uy
let Event_ActionMate_ID = Event_ActionEat_ID + 1uy
let Event_ActionMovement_ID = Event_ActionMate_ID + 1uy
let Event_Birth_ID = Event_ActionMovement_ID + 1uy
let Event_ComponentAdded_Controller_ID = Event_Birth_ID + 1uy
let Event_ComponentAdded_Eating_ID = Event_ComponentAdded_Controller_ID + 1uy
let Event_ComponentAdded_PlantGrowth_ID = Event_ComponentAdded_Eating_ID + 1uy
let Event_CreateEntity_ID = Event_ComponentAdded_PlantGrowth_ID + 1uy
let Event_Eaten_ID = Event_CreateEntity_ID + 1uy
let Event_FoodAllEaten_ID = Event_Eaten_ID + 1uy
let Event_KillAllEaten_ID = Event_FoodAllEaten_ID + 1uy
let Event_LocationChanged_ID = Event_KillAllEaten_ID + 1uy
let Event_Metabolize_ID = Event_LocationChanged_ID + 1uy
let Event_Movement_ID = Event_Metabolize_ID + 1uy
let Event_PlantGrowth_ID = Event_Movement_ID + 1uy
let Event_PlantReproduce_ID = Event_PlantGrowth_ID + 1uy
let Event_ScheduleEvent_ID = Event_PlantReproduce_ID + 1uy
let Event_Starving_ID = Event_ScheduleEvent_ID + 1uy

type ScheduleType =
    | RepeatFinite of uint32
    | RepeatIndefinitely
    | RunOnce

type Event_ActionEat = { EntityID:uint32 }
type Event_ActionMate = { EntityID:uint32 }
type Event_ActionMovement = { EntityID:uint32; Direction:MovementDirection }
type Event_Birth = { MomID:uint32; DadID:uint32 }
type Event_ComponentAdded_Controller = { EntityID:uint32; Component:Component }
type Event_ComponentAdded_Eating = { EntityID:uint32; Component:Component }
type Event_ComponentAdded_PlantGrowth = { EntityID:uint32; Component:Component }
type Event_CreateEntity = { Components:Component[] }
type Event_Eaten = { EaterID:uint32; EateeID:uint32; Quantity:uint32 }
type Event_FoodAllEaten = { EaterID:uint32; EateeID:uint32 }
type Event_KillAllEaten = { EaterID:uint32; EateeID:uint32 }
type Event_LocationChanged = { EntityID:uint32; Form:Component }
type Event_Metabolize = { EntityID:uint32 }
type Event_Movement = { EntityID:uint32; Direction:MovementDirection }
type Event_PlantGrowth = { EntityID:uint32 }
type Event_PlantReproduce = { EntityID:uint32 }
type Event_ScheduleEvent = { Frequency:uint32; Schedule:ScheduleType }
type Event_Starving = { EntityID:uint32 }

type GameEventTypes =
    | Action_Eat of Event_ActionEat
    | Action_Mate of Event_ActionMate
    | Action_Movement of Event_ActionMovement
    | Birth of Event_Birth
    | ComponentAdded_Controller of Event_ComponentAdded_Controller
    | ComponentAdded_Eating of Event_ComponentAdded_Eating
    | ComponentAdded_PlantGrowth of Event_ComponentAdded_PlantGrowth
    | CreateEntity of Event_CreateEntity
    | Eaten of Event_Eaten
    | Food_AllEaten of Event_FoodAllEaten
    | Kill_AllEaten of Event_KillAllEaten
    | LocationChanged of Event_LocationChanged
    | Metabolize of Event_Metabolize
    | Movement of Event_Movement
    | PlantRegrowth of Event_PlantGrowth
    | PlantReproduce of Event_PlantReproduce
    | ScheduleEvent of Event_ScheduleEvent * GameEventTypes
    | Starving of Event_Starving
    member me.EntityID =
        match me with
        | Action_Eat d -> d.EntityID
        | Action_Mate d -> d.EntityID
        | Action_Movement d -> d.EntityID
        | Birth d -> d.MomID
        | ComponentAdded_Controller d -> d.EntityID
        | ComponentAdded_Eating d -> d.EntityID
        | ComponentAdded_PlantGrowth d -> d.EntityID
        | CreateEntity d -> d.Components.[0].EntityID
        | Eaten d -> d.EaterID
        | Food_AllEaten d -> d.EateeID
        | Kill_AllEaten d -> d.EateeID
        | LocationChanged d -> d.EntityID
        | Metabolize d -> d.EntityID
        | Movement d -> d.EntityID
        | PlantRegrowth d -> d.EntityID
        | PlantReproduce d -> d.EntityID
        | ScheduleEvent (_,ge) -> ge.EntityID
        | Starving d -> d.EntityID
    member me.GameEventID = 
        match me with
        | Action_Eat _ -> Event_ActionEat_ID
        | Action_Mate _ -> Event_ActionMate_ID
        | Action_Movement _ -> Event_ActionMovement_ID
        | Birth _ -> Event_Birth_ID
        | ComponentAdded_Controller _ -> Event_ComponentAdded_Controller_ID
        | ComponentAdded_Eating _ -> Event_ComponentAdded_Eating_ID
        | ComponentAdded_PlantGrowth _ -> Event_ComponentAdded_PlantGrowth_ID
        | CreateEntity _ -> Event_CreateEntity_ID
        | Eaten _ -> Event_Eaten_ID
        | Food_AllEaten _ -> Event_FoodAllEaten_ID
        | Kill_AllEaten _ -> Event_KillAllEaten_ID
        | LocationChanged _ -> Event_LocationChanged_ID
        | Metabolize _ -> Event_Metabolize_ID
        | Movement _ -> Event_Movement_ID
        | PlantRegrowth _ -> Event_PlantGrowth_ID
        | PlantReproduce _ -> Event_PlantReproduce_ID
        | ScheduleEvent _ -> Event_ScheduleEvent_ID
        | Starving _ -> Event_Starving_ID
    member me.GameEventType() = 
        match me with
        | Action_Eat _ -> "Action_Eat"
        | Action_Mate _ -> "Action_Mate"
        | Action_Movement _ -> "Action_Movement"
        | Birth _ -> "Birth"
        | ComponentAdded_Controller _ -> "ComponentAdded_Controller"
        | ComponentAdded_Eating _ -> "ComponentAdded_Eating"
        | ComponentAdded_PlantGrowth _ -> "ComponentAdded_PlantGrowth"
        | CreateEntity _ -> "CreateEntity"
        | Eaten _ -> "Eaten"
        | Food_AllEaten _ -> "Food_AllEaten"
        | Kill_AllEaten _ -> "Kill_AllEaten"
        | LocationChanged _ -> "LocationChanged"
        | Metabolize _ -> "Metabolize"
        | Movement _ -> "Movement"
        | PlantRegrowth _ -> "PlantRegrowth"
        | PlantReproduce _ -> "PlantReproduce"
        | ScheduleEvent _ -> "ScheduleEvent"
        | Starving _ -> "Starving"
    member me.ToActionEat = 
        let (Action_Eat d) = me
        d
    member me.ToActionMate = 
        let (Action_Mate d) = me
        d
    member me.ToAction_Movement = 
        let (Action_Movement d) = me
        d
    member me.ToBirth = 
        let (Birth d) = me
        d
    member me.ToComponentAddedController =
        let (ComponentAdded_Controller d) = me
        d
    member me.ToComponentAddedEating =
        let (ComponentAdded_Eating d) = me
        d
    member me.ToComponentAddedPlantGrowth =
        let (ComponentAdded_PlantGrowth d) = me
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
    member me.ToLocationChanged =
        let (LocationChanged d) = me
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

