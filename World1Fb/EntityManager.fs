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
    
    member this.Entities = entDict.Entities
    member this.EntitiesWithComponent (ct:ComponentTypes) = entDict.EntitiesWithComponent ct
    member this.FormsAtLocation (l:LocationDataInt) = entDict.FormsAtLocation l
    member this.FormImpassableAtLocation (l:LocationDataInt) = entDict.FormImpassableAtLocation l
    member this.MaxEntityID = entDict.MaxEntityID
    member this.NewEntityID = entDict.NewEntityID
    member this.TryGetComponent (eid:uint32) (ct:ComponentTypes) = entDict.TryGetComponent eid ct
    member this.TryGetComponents (eid:uint32) (cts:ComponentTypes[]) = entDict.TryGetComponents eid cts

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

