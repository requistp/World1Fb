module Component
open ComponentEnums
open ControllerComponent
open EatingComponent
open FoodComponent
open FormComponent
open MatingComponent
open MovementComponent
open PlantGrowthComponent
open TerrainComponent

type Component = 
    | Controller of ControllerComponent
    | Eating of EatingComponent
    | Food of FoodComponent
    | Form of FormComponent
    | Mating of MatingComponent
    | Movement of MovementComponent
    | PlantGrowth of PlantGrowthComponent
    | Terrain of TerrainComponent
    member me.Copy newEID = 
        match me with
        | Controller d -> Controller { d with EntityID = newEID }
        | Eating d -> Eating { d with EntityID = newEID }
        | Food d -> Food { d with EntityID = newEID }
        | Form d -> Form { d with EntityID = newEID }
        | Mating d -> Mating { d with EntityID = newEID }
        | Movement d -> Movement { d with EntityID = newEID }
        | PlantGrowth d -> PlantGrowth { d with EntityID = newEID }
        | Terrain d -> Terrain { d with EntityID = newEID }
    member me.ComponentID =
        match me with
        | Controller _ -> ControllerComponentID
        | Eating _ -> EatingComponentID
        | Food _ -> FoodComponentID
        | Form _ -> FormComponentID
        | Mating _ -> MatingComponentID
        | Movement _ -> MovementComponentID
        | PlantGrowth _ -> PlantGrowthComponentID
        | Terrain _ -> TerrainComponentID
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

