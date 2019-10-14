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

    member this.Components = entDict.Components
    member this.Entities = entDict.Entities

    member this.EntitiesWithComponent (ct:ComponentTypes) = entDict.EntitiesWithComponent ct
    member this.FormsAtLocation (l:LocationDataInt) = entDict.FormsAtLocation l
    member this.GetComponent ct eid = entDict.GetComponent ct eid
    member this.Locations = entDict.Locations
    member this.NewEntityID = entDict.NewEntityID
    member this.TryGet eid = entDict.TryGet eid
    member this.TryGetComponent (eid:uint32) (ct:ComponentTypes) = entDict.TryGetComponent eid ct
    
    member this.TryGetComponents (eid:uint32) (cts:ComponentTypes[]) =
        cts |> Array.map (fun ct -> this.TryGetComponent eid ct) // Parallel worked but seemed to make it slower. rty later with timing in liver environment

    member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        let handleNewEntityEvents =
            let handleEvent (ct:AbstractComponent) =
                match ct.ComponentType with
                | Terrain -> () //evm.QueueEvent(Event_TerrainCreated(ct :?> TerrainComponent))
                | _ -> ()
            scl.NewEntities
            |> Array.iter (fun cts -> cts |> Array.iter (fun ct -> handleEvent ct)) //maybe test parallel after I have events

        entDict.ProcessSystemChangeLog scl

        handleNewEntityEvents

        (entDict.Entities, entDict.MaxEntityID)

    member this.Initialize =
        ()
