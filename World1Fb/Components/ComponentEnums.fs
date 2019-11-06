module ComponentEnums
open LocationTypes

let ControllerComponent_ID = 1uy
let EatingComponent_ID = 2uy
let FoodComponent_ID = 3uy
let FormComponent_ID = 4uy
let MatingComponent_ID = 5uy
let MovementComponent_ID = 6uy
let PlantGrowthComponent_ID = 7uy
let TerrainComponent_ID = 7uy


type FoodClassifications =
    | Meat
    | Plant


type FoodTypes =
    | Food_Carrot
    | Food_Grass
    | Food_Meat_Rabbit
    member this.Calories = // This is calories per Quantity
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
