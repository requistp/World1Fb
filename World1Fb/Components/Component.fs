module Component
open CalendarTimings
open ComponentEnums
open LocationTypes
open System

type ControllerComponent = { EntityID:uint32 }
    with 
    static member ID = 4uy

type EatingComponent = { EntityID:uint32; Calories:int; CaloriesPerDay:int; Foods:FoodTypes[]; Quantity:int; QuantityMax:int; QuantityPerAction:int }
    with 
    static member ID = 5uy
    member me.CaloriesPerMetabolize = Math.Clamp(convertAmountByFrequency me.CaloriesPerDay Day MetabolismFrequency,1,me.CaloriesPerDay)
    member me.CanEat (fd:FoodTypes) = me.Foods |> Array.contains fd    
    member me.QuantityPerMetabolize = Math.Clamp(convertAmountByFrequency me.QuantityMax Day MetabolismFrequency,1,me.QuantityMax)
    member me.QuantityRemaining = me.QuantityMax - me.Quantity
    member me.Update (quantityUpdate:int option) (caloriesUpdate:int option) = 
        {
            me with
                Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else me.Quantity
                Calories = if caloriesUpdate.IsSome then caloriesUpdate.Value else me.Calories
        }

type FoodComponent = { EntityID:uint32; FoodType:FoodTypes; Quantity:int; QuantityMax:int } 
    with 
    static member ID = 3uy
    member me.Update (foodTypeUpdate:FoodTypes option) (quantityUpdate:int option) (quantityMaxUpdate:int option) =
        {
            me with
                FoodType = if foodTypeUpdate.IsSome then foodTypeUpdate.Value else me.FoodType
                Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else me.Quantity
                QuantityMax = if quantityMaxUpdate.IsSome then quantityMaxUpdate.Value else me.QuantityMax
        }
    
type FormComponent = { EntityID:uint32; IsPassable:bool; Location:LocationDataInt; Name:string; Symbol:char }
    with 
    static member ID = 1uy
    member me.Update (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
        {
            me with
                IsPassable = if isPassableUpdate.IsSome then isPassableUpdate.Value else me.IsPassable
                Location = if locationUpdate.IsSome then locationUpdate.Value else me.Location
                Name = if nameUpdate.IsSome then nameUpdate.Value else me.Name
                Symbol = if symbolUpdate.IsSome then symbolUpdate.Value else me.Symbol
        }

type MovementComponent = { EntityID:uint32; MovesPerTurn:int }
    with 
    static member ID = 6uy

type PlantGrowthComponent = { EntityID:uint32; GrowsInTerrain:TerrainType[]; RegrowRate:float; ReproductionRate:float; ReproductionRange:int; ReproductionRequiredFoodQuantity:float }
    with 
    static member ID = 7uy

type TerrainComponent = { EntityID:uint32; Terrain:TerrainType }
    with 
    static member ID = 2uy

type Component = 
    | Controller of ControllerComponent
    | Eating of EatingComponent
    | Food of FoodComponent
    | Form of FormComponent
    | Movement of MovementComponent
    | PlantGrowth of PlantGrowthComponent
    | Terrain of TerrainComponent
    member me.Copy newEID = 
        match me with
        | Controller d -> Controller { d with EntityID = newEID }
        | Eating d -> Eating { d with EntityID = newEID }
        | Food d -> Food { d with EntityID = newEID }
        | Form d -> Form { d with EntityID = newEID }
        | Movement d -> Movement { d with EntityID = newEID }
        | PlantGrowth d -> PlantGrowth { d with EntityID = newEID }
        | Terrain d -> Terrain { d with EntityID = newEID }
    member me.ComponentID =
        match me with
        | Controller _ -> ControllerComponent.ID
        | Eating _ -> EatingComponent.ID
        | Food _ -> FoodComponent.ID
        | Form _ -> FormComponent.ID
        | Movement _ -> MovementComponent.ID
        | PlantGrowth _ -> PlantGrowthComponent.ID
        | Terrain _ -> TerrainComponent.ID
    member me.EntityID =
        match me with
        | Controller d -> d.EntityID
        | Eating d -> d.EntityID
        | Food d -> d.EntityID
        | Form d -> d.EntityID
        | Movement d -> d.EntityID
        | PlantGrowth d -> d.EntityID
        | Terrain d -> d.EntityID
    member me.IsPassable = 
        match me with
        | Form d -> d.IsPassable
        | _ -> true
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
    member me.ToMovement = 
        let (Movement d) = me
        d
    member me.ToPlantGrowth = 
        let (PlantGrowth d) = me
        d
    member me.ToTerrain = 
        let (Terrain d) = me
        d


