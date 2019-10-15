module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EntityDictionary
open EventManager
open FormComponent
open GameEvents
open LocationTypes

type EntityManager(evm:EventManager) =
    let entDict = new EntityDictionary()
    
    member _.Entities = entDict.Entities
    member _.EntitiesWithComponent (ct:ComponentTypes) = entDict.EntitiesWithComponent ct
    member _.MaxEntityID = entDict.MaxEntityID
    member _.NewEntityID = entDict.NewEntityID
    member _.TryGetComponent (ct:ComponentTypes) (eid:uint32) = entDict.TryGetComponent ct eid
    member _.TryGetComponents (cts:ComponentTypes[]) (eid:uint32) = entDict.TryGetComponents cts eid

    member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        //let handleNewEntityEvents (newscl:SystemChangeLog) =
        //    let handleEvent (ct:AbstractComponent) =
        //        match ct.ComponentType with
        //        | Terrain -> () //evm.QueueEvent(Event_TerrainCreated(ct :?> TerrainComponent))
        //        | _ -> ()
        //    newscl.NewEntities
        //    |> Array.iter (fun cts -> cts |> Array.iter (fun ct -> handleEvent ct)) //maybe test parallel after I have events
        //    newscl
        entDict.ProcessSystemChangeLog scl
        //|> handleNewEntityEvents

    member this.Initialize =
        ()


    //member this.Components = entDict.Components
    //member this.GetComponent ct eid = entDict.GetComponent ct eid
    //member this.Locations = entDict.Locations
    //member this.TryGet eid = entDict.TryGet eid

