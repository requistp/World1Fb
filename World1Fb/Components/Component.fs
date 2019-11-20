module Component
open CommonGenericFunctions
open ComponentEnums
open ControllerComponent
open EatingComponent
open FoodComponent
open FormComponent
open MatingComponent
open MemoryComponent
open MovementComponent
open PlantGrowthComponent
open TerrainComponent
open VisionComponent

type Component = 
    | Controller of ControllerComponent
    | Eating of EatingComponent
    | Food of FoodComponent
    | Form of FormComponent
    | Mating of MatingComponent
    | Movement of MovementComponent
    | PlantGrowth of PlantGrowthComponent
    | Terrain of TerrainComponent
    | Vision of VisionComponent

    static member GetID (c:Component) =
        c.ID
    static member GetComponentTypeID (c:Component) =
        c.ComponentTypeID
    static member GetEntityID (c:Component) =
        c.EntityID

    member me.ID = 
        match me with 
        | Controller d -> d.ID
        | Eating d -> d.ID
        | Food d -> d.ID
        | Form d -> d.ID
        | Mating d -> d.ID
        | Movement d -> d.ID
        | PlantGrowth d -> d.ID
        | Terrain d -> d.ID
        | Vision d -> d.ID
    member me.ComponentTypeID =
        match me with
        | Controller _ -> ControllerComponentID
        | Eating _ -> EatingComponentID
        | Food _ -> FoodComponentID
        | Form _ -> FormComponentID
        | Mating _ -> MatingComponentID
        | Movement _ -> MovementComponentID
        | PlantGrowth _ -> PlantGrowthComponentID
        | Terrain _ -> TerrainComponentID
        | Vision _ -> VisionComponentID
    member me.EntityID =
        match me with
        | Controller d -> d.EntityID
        | Eating d -> d.EntityID
        | Food d -> d.EntityID
        | Form d -> d.EntityID
        | Mating d -> d.EntityID
        | Movement d -> d.EntityID
        | PlantGrowth d -> d.EntityID
        | Terrain d -> d.EntityID
        | Vision d -> d.EntityID
    member me.ToController = 
        let (Controller d) = me
        d
    member me.ToEating = 
        let (Eating d) = me
        d
    member me.ToFood = 
        let (Food d) = me
        d
    member me.ToForm = 
        let (Form d) = me
        d
    member me.ToMating =
        let (Mating d) = me
        d
    member me.ToMovement = 
        let (Movement d) = me
        d
    member me.ToPlantGrowth = 
        let (PlantGrowth d) = me
        d
    member me.ToTerrain = 
        let (Terrain d) = me
        d
    member me.ToVision = 
        let (Vision d) = me
        d

let EntityID (c:Component) =
    match c with
    | Controller d -> d.EntityID
    | Form d -> d.EntityID
    | Component.Movement d -> d.EntityID
    | Terrain d -> d.EntityID

let ToEating (Eating c) = c
let ToFood (Food c) = c
let ToForm (Form c) = c
let ToMating (Mating c) = c
let ToPlantGrowth (PlantGrowth c) = c

let Copy (newEID:EntityID) (newCID:ComponentID) (c:Component) =
    match c with
    | Controller d -> Controller { d with ID = newCID; EntityID = newEID }
    | Eating d -> Eating { d with ID = newCID; EntityID = newEID }
    | Food d -> Food { d with ID = newCID; EntityID = newEID }
    | Form d -> Form { d with ID = newCID; EntityID = newEID }
    | Mating d -> Mating { d with ID = newCID; EntityID = newEID }
    | Movement d -> Movement { d with ID = newCID; EntityID = newEID }
    | PlantGrowth d -> PlantGrowth { d with ID = newCID; EntityID = newEID }
    | Terrain d -> Terrain { d with ID = newCID; EntityID = newEID }
    | Vision d -> Vision { d with ID = newCID; EntityID = newEID }