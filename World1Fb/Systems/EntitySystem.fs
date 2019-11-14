module EntitySystem
open Component
open ComponentEnums
open EventTypes
open GameManager
open SystemManager


type EntitySystem(description:string, game:Game, isActive:bool) =
    inherit AbstractSystem(description,isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onCreateEntity round (ge:GameEventTypes) =
        let e = ge.ToCreateEntity
        let checkComponent (c:Component) =
            match c.ComponentID with 
            | x when x = ControllerComponentID -> evm.RaiseEvent (ComponentAdded_Controller { EntityID=c.EntityID; Component=c })
            | x when x = EatingComponentID -> evm.RaiseEvent (ComponentAdded_Eating { EntityID=c.EntityID; Component=c })
            | x when x = PlantGrowthComponentID -> evm.RaiseEvent (ComponentAdded_PlantGrowth { EntityID=c.EntityID; Component=c })
            | _ -> ()
        e.Components |> Array.Parallel.iter (fun c -> checkComponent c)
        enm.CreateEntity (e.Components)

    override me.Initialize =
        evm.RegisterListener me.Description Event_CreateEntity_ID (me.TrackTask me.onCreateEntity)
        base.SetToInitialized

    override me.Update round = 
        ()


        
