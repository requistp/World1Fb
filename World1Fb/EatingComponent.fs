module EatingComponent
open AbstractComponent
open FoodComponent
open System


type EatingComponent(eid:uint32, quantityPerAction:int, foods:FoodTypes[], caloriesMax:int, caloriesCurrent:int) = 
    inherit AbstractComponent(eid,Eating)

    member _.Foods = foods
    member _.QuantityPerAction = quantityPerAction
    member _.Calories_Current = caloriesCurrent
    member _.Calories_Max = caloriesMax


    
//type EatingComponent_Change(eid:uint32, invalid:string option, calories:int) =
//    inherit AbstractComponentChange(Eating,eid,invalid)
    
//    member _.Calories = calories

//    override this.AddChange (a:AbstractComponent) =
//        let c = a :?> EatingComponent
//        EatingComponent(eid, c.QuantityPerAction, c.Foods, c.Calories_Max, Math.Clamp(this.Calories+c.Calories_Current,0,c.Calories_Max)) :> AbstractComponent

//    override this.Invalidate (reason:string) =
//        EatingComponent_Change(this.EntityID, Some reason, this.Calories) :> AbstractComponentChange
    
//    new (eid:uint32, calories:int) = EatingComponent_Change(eid, None, calories)
        
