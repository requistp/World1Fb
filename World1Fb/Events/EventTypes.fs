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

type GameEventTypes = 
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

type ScheduleType =
    | RepeatFinite of RoundNumber
    | RepeatIndefinitely
    | RunOnce

type GameEventData =
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
    | ScheduleEvent of RoundNumber * ScheduleType * GameEventData
    | Starving of EatingComponent

let GetGameEvent (ge:GameEventData) =
    match ge with
    | Action_Eat _ -> Event_ActionEat
    | Action_Mate _ -> Event_ActionMate
    | Action_Movement _ -> Event_ActionMovement
    | Birth _ -> Event_Birth
    | ComponentAdded_Controller _ -> Event_ComponentAdded_Controller
    | ComponentAdded_Eating _ -> Event_ComponentAdded_Eating
    | ComponentAdded_PlantGrowth _ -> Event_ComponentAdded_PlantGrowth
    | CreateEntity _ -> Event_CreateEntity
    | Eaten _ -> Event_Eaten
    | Food_AllEaten _ -> Event_FoodAllEaten
    | Kill_AllEaten _ -> Event_KillAllEaten
    | LocationChanged _ -> Event_LocationChanged
    | Metabolize _ -> Event_Metabolize
    | PlantRegrowth _ -> Event_PlantGrowth
    | PlantReproduce _ -> Event_PlantReproduce
    | ScheduleEvent _ -> Event_ScheduleEvent
    | Starving _ -> Event_Starving

let rec GetGameEvent_EntityID (ge:GameEventData) =
    match ge with
    | Action_Eat d -> d.EntityID
    | Action_Mate d -> d.EntityID
    | Action_Movement (f,_) -> f.EntityID
    | Birth (m,_) -> m.EntityID
    | ComponentAdded_Controller d -> d.EntityID
    | ComponentAdded_Eating d -> d.EntityID
    | ComponentAdded_PlantGrowth d -> d.EntityID
    | CreateEntity d -> GetComponentEntityID d.[0]
    | Eaten (eat,_) -> eat.EntityID
    | Food_AllEaten (eat,_) -> eat.EntityID
    | Kill_AllEaten (eat,_) -> eat.EntityID
    | LocationChanged d -> d.EntityID
    | Metabolize d -> d.EntityID
    | PlantRegrowth d -> d.EntityID
    | PlantReproduce d -> d.EntityID
    | ScheduleEvent (_,_,ge) -> GetGameEvent_EntityID ge
    | Starving d -> d.EntityID


