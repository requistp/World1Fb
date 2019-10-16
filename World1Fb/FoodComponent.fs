module FoodComponent
open AbstractComponent

type FoodClassifications =
    | Meat
    | Plant

type FoodTypes =
    | Carrot
    | Grass
    | Meat_Rabbit
    member this.Calories = // This is calories per Quantity
        match this with 
        | Carrot -> 2
        | Grass -> 1
        | Meat_Rabbit -> 3
    member this.Classification =
        match this with
        | Carrot | Grass -> Plant
        | Meat_Rabbit -> Meat
    member this.Symbol =
        match this.Classification with
        | Plant -> Some '!'
        | Meat -> None


type FoodComponent(eid:uint32, foodType:FoodTypes, quantity:int) = 
    inherit AbstractComponent(eid,Food)

    member _.FoodType = foodType
    member _.Quantity = quantity

    
//type FoodComponent_Change(eid:uint32, invalid:string option, quantity:int) =
//    inherit AbstractComponentChange(Food,eid,invalid)

//    member _.Quantity = quantity

//    override this.AddChange (a:AbstractComponent) =
//        let c = a :?> FoodComponent
//        FoodComponent(eid, c.FoodType, this.Quantity + c.Quantity) :> AbstractComponent

//    override this.Invalidate (reason:string) =
//        FoodComponent_Change(this.EntityID, Some reason, this.Quantity) :> AbstractComponentChange

//    new (eid:uint32, quantity:int) = FoodComponent_Change(eid, None, quantity)

