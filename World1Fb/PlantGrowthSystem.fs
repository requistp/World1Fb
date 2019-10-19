module PlantGrowthSystem
open AbstractSystem
open EntityDictionary
open FoodComponent
open PlantGrowthComponent
open GameEvents
open GameManager
open SystemManager
open System

type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    //member private this.onRegrowth (next:NextEntityDictionary) (ge:AbstractGameEvent) =
    //    let e = ge :?> Event_PlantRegrowth
    //    if (e.EntityID |> next.HasComponent<FoodComponent>) then ()
    //    //match (e.EntityID |> game.EntityManager.GetComponent<PlantGrowthComponent>).FoodType.KillOnAllEaten with
    //    //| false -> Ok None
    //    //| true -> game.EventManager.QueueEvent (Event_Kill_AllEaten(e.EntityID))
    //    Ok None

    member private this.UpdateRegrowth r =
        game.EntityManager.EntitiesWithComponent PlantGrowthComponent.Type
        |> game.EntityManager.GetComponent<PlantGrowthComponent>
        |> Array.Parallel.choose (fun c -> if (c.Timer.Execute r) then Some c else None) // Test this!!! Used to be Array.filter (fun c -> c.Timer.Execute r)
        |> Array.iter (fun c -> game.EventManager.QueueEvent (Event_PlantRegrowth(c.EntityID)))
    
    override this.Initialize = 
        //game.EventManager.RegisterListener PlantRegrowth this.onRegrowth
        base.SetToInitialized

    override this.Update r = 
        this.UpdateRegrowth r

