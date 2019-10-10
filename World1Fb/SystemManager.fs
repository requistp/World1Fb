module SystemManager
open AbstractComponent
open EntityComponentManager

type SystemChangeLog = 
    {
        Items : AbstractComponentChange[]
        Sum : AbstractComponentChange[]
    } with 
    static member empty = 
        { 
            Items = Array.empty<AbstractComponentChange>
            Sum = Array.empty<AbstractComponentChange>
        }
    static member New (c:AbstractComponentChange[],s:AbstractComponentChange[]) =
        {   
            Items = c
            Sum = s
        }  
    static member Add (scl1:SystemChangeLog) (scl2:SystemChangeLog) = 
        {
            Items = scl2.Items |> Array.append scl1.Items
            Sum = scl2.Sum |> Array.append scl1.Sum
        }


[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false
    let mutable _pendingChanges = Array.empty<AbstractComponentChange>

    let updateSumOfChanges (map:Map<uint32,AbstractComponentChange>) (c:AbstractComponentChange) =
        match map.ContainsKey(c.EntityID) with
        | false -> map.Add(c.EntityID,c)
        | true -> let i = map.Item(c.EntityID)
                  map.Remove(c.EntityID).Add(c.EntityID,i.AddChange(c))
   
    let sumOfPendingChanges (pc:AbstractComponentChange[]) = 
        pc
        |> Array.fold (fun map c -> updateSumOfChanges map c) Map.empty 
        |> Map.toArray
        |> Array.map (fun tup -> snd tup)

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.AppendChange a = _pendingChanges <- Array.append _pendingChanges [|a|]
    member internal this.SetToInitialized = _isInitialized <- true
    member internal this.ConsolidateChanges =
        let c = _pendingChanges
        _pendingChanges <- Array.empty
        let s = sumOfPendingChanges c
        SystemChangeLog.New(c,s)

    abstract member Initialize : unit
    abstract member Update : SystemChangeLog


type SystemManager() =
    let mutable _systems = Array.empty<AbstractSystem>

    let applyChanges (ecd:EntityComponentData) (c:AbstractComponentChange) =
        match c.EntityID |> Entity.TryGetComponent ecd.Entities c.ComponentType with
        | None -> ecd
        | Some a -> a
                    |> c.AddChange
                    |> Entity.ReplaceComponent ecd c.EntityID

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member this.RegisterSystems (sl:AbstractSystem[]) =
        _systems <- sl
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems (ecd:EntityComponentData) =
        let scl = 
            this.ActiveAndInitialized 
            |> Array.Parallel.map (fun x -> x.Update) 
            |> Array.fold (fun scl c -> SystemChangeLog.Add scl c) SystemChangeLog.empty

        let newecd = 
            scl.Sum
            |> Array.sortBy (fun s -> s.EntityID)
            |> Array.fold (fun d s -> applyChanges d s) ecd

        (newecd,scl)

