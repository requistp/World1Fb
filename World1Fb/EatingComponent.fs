module g
open AbstractComponent


type EatingComponent(eid:uint32, quantityPerAction:int) = 
    inherit AbstractComponent(eid,Eating)

    member _.QuantityPerAction = quantityPerAction

    
