module EntitySystem
open Component
open ComponentEnums
open EventManager
open EventTypes
open SystemManager
open EntityManager


type EntitySystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onCreateEntity round (CreateEntity cts:GameEventData) =
        let checkComponentForEvents (c:Component) =
            match (GetComponentType c) with 
            | x when x = ControllerComponent -> evm.RaiseEvent (ComponentAdded_Controller (ToController c))
            | x when x = EatingComponent -> evm.RaiseEvent (ComponentAdded_Eating (ToEating c))
            | x when x = PlantGrowthComponent -> evm.RaiseEvent (ComponentAdded_PlantGrowth (ToPlantGrowth c))
            | _ -> ()
        cts |> Array.iter checkComponentForEvents
        enm.CreateEntity cts

    override me.Initialize =
        evm.RegisterListener me.Description Event_CreateEntity (me.TrackTask me.onCreateEntity)
        base.SetToInitialized

    override me.Update round = 
        ()


        
