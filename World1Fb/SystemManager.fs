module SystemManager
open AbstractComponent
open EntityComponentManager
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
    static member New (ccs:AbstractComponentChange[]) (nes:AbstractComponent[][]) =
        {   
            ComponentChanges = ccs
            NewEntities = nes
        }  
    static member Add (scl1:SystemChangeLog) (scl2:SystemChangeLog) = 
        {
            ComponentChanges = scl2.ComponentChanges |> Array.append scl1.ComponentChanges
            NewEntities = scl2.NewEntities |> Array.append scl1.NewEntities
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


type SystemManager(ev:EventManager, em:EntityManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member private this.ApplyChangeLogs =
        let scl =
            this.ActiveAndInitialized 
            |> Array.map (fun x -> x.Update) 
            |> Array.fold (fun scl c -> SystemChangeLog.Add scl c) SystemChangeLog.empty
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
            |> Array.iter (fun acc -> ev.QueueEvent(Event_Entity_ReplaceComponent(acc)))
        let applyCreateEntities = 
            scl.NewEntities |> Array.iter (fun acs -> ev.QueueEvent(Event_Entity_Create(acs)))

        applyComponentChanges
        applyCreateEntities
        scl

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems =
        this.ApplyChangeLogs

