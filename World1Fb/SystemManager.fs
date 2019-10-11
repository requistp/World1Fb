module SystemManager
open AbstractComponent
open EntityManager
open EventManager
open GameEvents

type SystemChangeLog = 
    {
        ComponentChanges : AbstractComponentChange[]
        NewEntities : AbstractComponent[][]
    } with 
    static member empty = 
        { 
            ComponentChanges = Array.empty
            NewEntities = Array.empty
        }
    member this.Append (scl:SystemChangeLog) = 
        {
            ComponentChanges = scl.ComponentChanges |> Array.append this.ComponentChanges
            NewEntities = scl.NewEntities |> Array.append this.NewEntities
        }
    member this.Add_ComponentChange (c:AbstractComponentChange) =
        { this with ComponentChanges = Array.append this.ComponentChanges [|c|] }
    member this.Add_NewEntity (ne:AbstractComponent[]) =
        { this with NewEntities = Array.append this.NewEntities [|ne|] }

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false
    let mutable _systemChangeLog = SystemChangeLog.empty

    member _.IsActive = isActive
    member _.IsInitialized = _isInitialized

    member internal this.ChangeLog_AddComponentChange c = _systemChangeLog <- _systemChangeLog.Add_ComponentChange c
    member internal this.ChangeLog_NewEntity ne = _systemChangeLog <- _systemChangeLog.Add_NewEntity ne
    member internal this.ChangeLog_PackageAndClose =
        let scl = _systemChangeLog
        _systemChangeLog <- SystemChangeLog.empty
        scl
    member internal this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    abstract member Update : SystemChangeLog


type SystemManager(evm:EventManager, enm:EntityManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member private this.ApplyChangeLogs =
        let scl =
            this.ActiveAndInitialized 
            |> Array.map (fun x -> x.Update) 
            |> Array.fold (fun scl c -> c.Append scl) SystemChangeLog.empty
            
        let applyComponentChanges = 
            let sumOfComponentChanges (ccs:AbstractComponentChange[]) = 
                let compileChanges (map:Map<uint32,AbstractComponentChange>) (c:AbstractComponentChange) =
                    match map.ContainsKey(c.EntityID) with
                    | false -> map.Add(c.EntityID,c)
                    | true -> let i = map.Item(c.EntityID)
                              map.Remove(c.EntityID).Add(c.EntityID,i.AddChange(c))
                ccs
                |> Array.fold (fun map c -> compileChanges map c) Map.empty 
                |> Map.toArray
                |> Array.map (fun tup -> snd tup)   
                
            scl.ComponentChanges
            |> sumOfComponentChanges
            //|> Array.iter (fun acc -> evm.QueueEvent(Event_Entity_ComponentChanges(acc)))
            
        applyComponentChanges
        evm.QueueEvent(Event_Entity_Creates(scl.NewEntities))
        scl

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems =
        this.ApplyChangeLogs

