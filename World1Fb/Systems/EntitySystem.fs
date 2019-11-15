module EntitySystem
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open SystemManager
open Entities

type EntitySystem(description:string, isActive:bool, enm:Entities, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onCreateEntity round (ge:GameEventTypes) =
        let e = ge.ToCreateEntity
        let checkComponentForEvents (c:Component) =
            match c.ComponentID with 
            | x when x = ControllerComponentID -> evm.RaiseEvent (ComponentAdded_Controller { EntityID=c.EntityID; Component=c })
            | x when x = EatingComponentID -> evm.RaiseEvent (ComponentAdded_Eating { EntityID=c.EntityID; Component=c })
            | x when x = PlantGrowthComponentID -> evm.RaiseEvent (ComponentAdded_PlantGrowth { EntityID=c.EntityID; Component=c })
            | _ -> ()
        e.Components |> Array.Parallel.iter (fun c -> checkComponentForEvents c)
        enm.CreateEntity e.Components

    override me.Initialize =
        evm.RegisterListener me.Description Event_CreateEntity_ID (me.TrackTask me.onCreateEntity)
        base.SetToInitialized

    override me.Update round = 
        ()


        
