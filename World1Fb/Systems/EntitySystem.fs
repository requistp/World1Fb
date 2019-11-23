module EntitySystem
open Component
open ComponentEnums
open EventManager
open EventTypes
open SystemManager
open EntityManager


type EntitySystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onCreateEntity round (CreateEntity cts:GameEventTypes) =
        let checkComponentForEvents (c:Component) =
            match c.ComponentTypeID with 
            | x when x = ControllerComponentID -> evm.RaiseEvent (ComponentAdded_Controller (ToController c))
            | x when x = EatingComponentID -> evm.RaiseEvent (ComponentAdded_Eating (ToEating c))
            | x when x = PlantGrowthComponentID -> evm.RaiseEvent (ComponentAdded_PlantGrowth (ToPlantGrowth c))
            | _ -> ()
        cts |> Array.Parallel.iter checkComponentForEvents
        enm.CreateEntity round cts

    override me.Initialize =
        evm.RegisterListener me.Description Event_CreateEntity_ID (me.TrackTask me.onCreateEntity)
        base.SetToInitialized

    override me.Update round = 
        ()


        
