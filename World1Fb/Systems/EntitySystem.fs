module EntitySystem
open Component
open ComponentEnums
open EventTypes
open GameManager
open SystemManager


type EntitySystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onCreateEntity (ge:GameEventTypes) =
        let e = ge.ToCreateEntity
        let checkComponent (c:Component) =
            match c.ComponentID with 
            | x when x = EatingComponentID -> evm.RaiseEvent (ComponentAdded_Eating { EntityID=c.EntityID; Component=c })
            | x when x = PlantGrowthComponentID -> evm.RaiseEvent (ComponentAdded_PlantGrowth { EntityID=c.EntityID; Component=c })
            | _ -> ()
        e.Components |> Array.Parallel.iter (fun c -> checkComponent c)
        enm.CreateEntity (e.Components)

    override me.Initialize =
        evm.RegisterListener me.ToString Event_CreateEntity_ID me.onCreateEntity
        base.SetToInitialized

    override _.ToString = "EntityManager"

    override me.Update round = 
        ()


        
