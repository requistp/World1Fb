module EventTypes
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent

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

type Event_ActionEat = { EntityID:EntityID }
type Event_ActionMate = { EntityID:EntityID }
type Event_ActionMovement = { EntityID:EntityID; Direction:MovementDirection }
type Event_Birth = { MomID:EntityID; DadID:EntityID }
type Event_ComponentAdded_Controller = { EntityID:EntityID; Component:Component }
type Event_ComponentAdded_Eating = { EntityID:EntityID; Component:Component }
type Event_ComponentAdded_PlantGrowth = { EntityID:EntityID; Component:Component }
type Event_CreateEntity = { Components:Component[] }
type Event_Eaten = { EaterID:EntityID; EateeID:EntityID; Quantity:int }
type Event_FoodAllEaten = { EaterID:EntityID; EateeID:EntityID }
type Event_KillAllEaten = { EaterID:EntityID; EateeID:EntityID }
type Event_LocationChanged = { EntityID:EntityID; Form:FormComponent }
type Event_Metabolize = { EntityID:EntityID }
type Event_Movement = { EntityID:EntityID; Direction:MovementDirection }
type Event_PlantGrowth = { EntityID:EntityID }
type Event_PlantReproduce = { EntityID:EntityID }
type Event_ScheduleEvent = { Frequency:uint32; Schedule:ScheduleType }
type Event_Starving = { EntityID:EntityID }


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

let EntityID (ge:GameEventTypes) = 
    match ge with
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

let ToEvent_KillAllEaten (Kill_AllEaten ge) = ge

//let ToController (Controller c) = c
//let x = Action_Eat { EntityID = 0u }
//let y = Terrain { ID = 0u; EntityID = 1u; Terrain = Dirt }

//let a = EntityID x
//let b = Component.EntityID y


