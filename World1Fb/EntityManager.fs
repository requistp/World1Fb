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

    member this.ComponentDictionary = entDict.Components
    member this.Entities = entDict.Entities
    member this.EntitiesAtLocation (l:LocationDataInt) = 
        match entDict.Locations.ContainsKey(l) with
        | true -> entDict.Locations.Item(l)
        | false -> Array.empty
    member this.EntitiesWithComponent (ct:ComponentTypes) = entDict.EntitiesWithComponent ct
    member this.GetComponent ct eid = entDict.GetComponent ct eid
    member this.LocationDictionary = entDict.Locations
    member this.NewEntityID = entDict.NewEntityID
    
    member this.FormsAtLocation (l:LocationDataInt) =
        this.EntitiesAtLocation l
        |> Array.Parallel.map (fun eid -> (this.GetComponent Form eid) :?> FormComponent)

    member this.TryGet eid =
        entDict.Entities.ContainsKey(eid) |> TrueSomeFalseNone (entDict.Entities.Item(eid))

    member this.TryGetComponent (eid:uint32) (ct:ComponentTypes) = 
        let tryGetComponent (cts:AbstractComponent[]) = 
            match cts |> Array.filter (fun c -> c.ComponentType = ct) with
            | [||] -> None
            | l -> Some l.[0]
        eid 
        |> this.TryGet 
        |> Option.bind tryGetComponent
    
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
