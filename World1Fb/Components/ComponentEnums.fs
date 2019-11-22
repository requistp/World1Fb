module ComponentEnums
open CommonGenericFunctions
open LocationTypes

let ControllerComponentID = ComponentTypeID(1uy)
let EatingComponentID = ComponentTypeID(ControllerComponentID.ToByte + 1uy)
let FoodComponentID = ComponentTypeID(EatingComponentID.ToByte + 1uy)
let FormComponentID = ComponentTypeID(FoodComponentID.ToByte + 1uy)
let MatingComponentID = ComponentTypeID(FormComponentID.ToByte + 1uy)
let MemoryComponentID = ComponentTypeID(MatingComponentID.ToByte + 1uy)
let MovementComponentID = ComponentTypeID(MemoryComponentID.ToByte + 1uy)
let PlantGrowthComponentID = ComponentTypeID(MovementComponentID.ToByte + 1uy)
let TerrainComponentID = ComponentTypeID(PlantGrowthComponentID.ToByte + 1uy)
let VisionComponentID = ComponentTypeID(TerrainComponentID.ToByte + 1uy)


type FoodClassifications =
    | Meat
    | Plant


type FoodTypes =
    | Food_Carrot
    | Food_Grass
    | Food_Meat_Rabbit
    member this.Calories = // This is calories per quantity (eaten)
        match this with 
        | Food_Carrot -> 2
        | Food_Grass -> 1
        | Food_Meat_Rabbit -> 3
    member this.Classification =
        match this with
        | Food_Carrot | Food_Grass -> Plant
        | Food_Meat_Rabbit -> Meat
    member this.KillOnAllEaten =
        match this with 
        | Food_Grass | Food_Meat_Rabbit -> false
        | Food_Carrot -> true
    member this.Symbol =
        match this.Classification with
        | Plant -> Some '!'
        | Meat -> None


type MatingStatus =
    | Female
    | Female_Pregnant
    | Male


type MovementDirection =
    | North
    | East
    | South
    | West
    member this.X_change = 
        match this with
        | North | South -> 0
        | East -> 1
        | West -> -1
    member this.Y_change = 
        match this with
        | East | West -> 0
        | North -> -1
        | South -> 1
    member this.Z_change = 
        match this with
        | _ -> 0
    member this.AddToLocation (l:LocationDataInt) =
        { X = l.X + this.X_change; Y = l.Y + this.Y_change; Z = l.Z + this.Z_change}


type Species = 
    | Rabbit
    | NotRabbit
    member me.MaxMatingFrequency =
        match me with
        | Rabbit -> 10u
        | NotRabbit -> 100u
    member me.Gestation =
        match me with
        | Rabbit -> 15u
        | NotRabbit -> 200u


type TerrainType = 
    | Dirt 
    | Rock
    | Sand
    member this.IsPassable = 
        match this with
        | Dirt | Sand -> true
        | Rock -> false
    member this.Symbol = 
        match this with
        | Dirt -> '.'
        | Sand -> ','
        | Rock -> '#'
