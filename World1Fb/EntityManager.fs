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
    member this.GetAllWithComponent (ct:ComponentTypes) = entDict.GetAllWithComponent ct
    member this.LocationDictionary = entDict.Locations
    member this.MaxEntityID = entDict.MaxEntityID
    member this.NewEntityID = entDict.NewEntityID

    member this.LocationIsPassable (l:LocationDataInt) =
        let impassableFormAtLocation = 
            //|> Array.map (fun ac -> (ac :?> FormComponent))
            //|> Array.exists (fun f -> not f.IsPassable)
            false
        not impassableFormAtLocation

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
            |> Array.iter (fun cts -> cts |> Array.iter (fun ct -> handleEvent ct))

        entDict.ProcessSystemChangeLog scl
        handleNewEntityEvents
        (entDict.Entities,this.MaxEntityID)

    member this.Initialize =
        ()
