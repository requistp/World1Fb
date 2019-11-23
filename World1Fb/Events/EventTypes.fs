module EventTypes
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EatingComponent
open FoodComponent
open FormComponent
open MatingComponent
open PlantGrowthComponent

type GameEventTypeIDs = 
    | Event_ActionEat
    | Event_ActionMate
    | Event_ActionMovement
    | Event_Birth
    | Event_ComponentAdded_Controller
    | Event_ComponentAdded_Eating
    | Event_ComponentAdded_PlantGrowth
    | Event_CreateEntity
    | Event_Eaten
    | Event_FoodAllEaten
    | Event_KillAllEaten
    | Event_LocationChanged
    | Event_Metabolize
    | Event_Movement
    | Event_PlantGrowth
    | Event_PlantReproduce
    | Event_ScheduleEvent
    | Event_Starving

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
    | RepeatFinite of RoundNumber
    | RepeatIndefinitely
    | RunOnce

type GameEventTypes =
    | Action_Eat of EatingComponent
    | Action_Mate of MatingComponent
    | Action_Movement of FormComponent * MovementDirection
    | Birth of mom:MatingComponent * dad:MatingComponent
    | ComponentAdded_Controller of ControllerComponent
    | ComponentAdded_Eating of EatingComponent
    | ComponentAdded_PlantGrowth of PlantGrowthComponent
    | CreateEntity of Component[]
    | Eaten of EatingComponent * FoodComponent
    | Food_AllEaten of EatingComponent * FoodComponent
    | Kill_AllEaten of EatingComponent * FoodComponent
    | LocationChanged of FormComponent
    | Metabolize of EatingComponent
    | PlantRegrowth of PlantGrowthComponent
    | PlantReproduce of PlantGrowthComponent
    | ScheduleEvent of RoundNumber * ScheduleType * GameEventTypes
    | Starving of EatingComponent
    member me.EntityID =
        match me with
        | Action_Eat d -> d.EntityID
        | Action_Mate d -> d.EntityID
        | Action_Movement (f,_) -> f.EntityID
        | Birth (m,_) -> m.EntityID
        | ComponentAdded_Controller d -> d.EntityID
        | ComponentAdded_Eating d -> d.EntityID
        | ComponentAdded_PlantGrowth d -> d.EntityID
        | CreateEntity d -> d.[0].EntityID
        | Eaten (eat,_) -> eat.EntityID
        | Food_AllEaten (eat,_) -> eat.EntityID
        | Kill_AllEaten (eat,_) -> eat.EntityID
        | LocationChanged d -> d.EntityID
        | Metabolize d -> d.EntityID
        | PlantRegrowth d -> d.EntityID
        | PlantReproduce d -> d.EntityID
        | ScheduleEvent (_,_,ge) -> ge.EntityID
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
        | PlantRegrowth _ -> "PlantRegrowth"
        | PlantReproduce _ -> "PlantReproduce"
        | ScheduleEvent _ -> "ScheduleEvent"
        | Starving _ -> "Starving"



