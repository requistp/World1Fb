module Component
open CommonGenericFunctions
open ComponentEnums
open ControllerComponent
open EatingComponent
open FoodComponent
open FormComponent
open MatingComponent
open MovementComponent
open PlantGrowthComponent
open TerrainComponent
open VisionComponent
open System.Runtime.CompilerServices


//[<IsByRefLike; Struct>]
//[<Struct>]
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

let ToController (Controller c) = c
let ToEating (Eating c) = c
let ToFood (Food c) = c
let ToForm (Form c) = c
let ToMating (Mating c) = c
let ToMovement (Movement c) = c
let ToPlantGrowth (PlantGrowth c) = c
let ToTerrain (Terrain c) = c
let ToVision (Vision c) = c

let Copy (newEID:EntityID) (idGen:unit->ComponentID) (round:RoundNumber) (c:Component) =
    match c with
    | Controller d -> Controller(ControllerComponent(idGen(), newEID, d.ControllerType, Idle, [|Idle|], [|Idle|]))
    | Eating d -> Eating(EatingComponent(idGen(), newEID, d.Calories, d.CaloriesPerDay, d.Foods, d.Quantity, d.QuantityMax, d.QuantityPerAction)) // Eating { d with ID = idGen(); EntityID = newEID }
    | Food d -> Food(FoodComponent(idGen(), newEID, d.FoodType, d.Quantity, d.QuantityMax)) // { d with ID = idGen(); EntityID = newEID }
    | Form d -> Form(FormComponent(idGen(), newEID, round, d.CanSeePast, d.IsPassable, d.Location, d.Name, d.Symbol)) //Form { d with ID = idGen(); EntityID = newEID; Born = round }
    | Mating d -> Mating(MatingComponent(idGen(), newEID, d.ChanceOfReproduction, d.LastMatingAttempt, d.MatingStatus, d.Species)) //Mating { d with ID = idGen(); EntityID = newEID }
    | Movement d -> Movement(MovementComponent(idGen(), newEID, d.MovesPerTurn)) // { d with ID = idGen(); EntityID = newEID }
    | PlantGrowth d -> PlantGrowth(PlantGrowthComponent(idGen(), newEID, d.GrowsInTerrain, d.RegrowRate, d.ReproductionRate, d.ReproductionRange, d.ReproductionRequiredFoodQuantity)) //PlantGrowth { d with ID = idGen(); EntityID = newEID }
    | Terrain d -> Terrain(TerrainComponent(idGen(), newEID, d.Terrain))
    | Vision d -> Vision(VisionComponent(idGen(), newEID, d.LocationsWithinRange, d.Range, d.RangeTemplate, d.VisionCalculationType, d.ViewedHistory, d.VisibleLocations))

let GetComponentID (c:Component) =
    match c with 
    | Controller d -> d.ID
    | Eating d -> d.ID
    | Food d -> d.ID
    | Form d -> d.ID
    | Mating d -> d.ID
    | Movement d -> d.ID
    | PlantGrowth d -> d.ID
    | Terrain d -> d.ID
    | Vision d -> d.ID

let GetComponentType (c:Component) =
    match c with
    | Controller _ -> ControllerComponentType
    | Eating _ -> EatingComponentType
    | Food _ -> FoodComponentType
    | Form _ -> FormComponentType
    | Mating _ -> MatingComponentType
    | Movement _ -> MovementComponentType
    | PlantGrowth _ -> PlantGrowthComponentType
    | Terrain _ -> TerrainComponentType
    | Vision _ -> VisionComponentType

let GetComponentEntityID (c:Component) =
    match c with
    | Controller d -> d.EntityID
    | Eating d -> d.EntityID
    | Food d -> d.EntityID
    | Form d -> d.EntityID
    | Mating d -> d.EntityID
    | Movement d -> d.EntityID
    | PlantGrowth d -> d.EntityID
    | Terrain d -> d.EntityID
    | Vision d -> d.EntityID


