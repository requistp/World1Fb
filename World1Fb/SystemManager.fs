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
            Items = Array.empty
            Sum = Array.empty
        }
    static member New (c:AbstractComponentChange[],s:AbstractComponentChange[]) =
        {   
            Items = c //|> Array.Parallel.map (fun x -> x :> AbstractComponentChange)
            Sum = s //|> Array.Parallel.map (fun x -> x :> AbstractComponentChange)
        }  
    member this.Add scl2 = 
        {
            Items = scl2.Items |> Array.append this.Items
            Sum = scl2.Sum |> Array.append this.Sum
        }


[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true

    member private this.updateSumOfChanges (map:Map<uint32,AbstractComponentChange>) (c:AbstractComponentChange) =
        match map.ContainsKey(c.EntityID) with
        | false -> map.Add(c.EntityID,c)
        | true -> let i = map.Item(c.EntityID)
                  map.Remove(c.EntityID).Add(c.EntityID,i.AddChange(c))
    
    member internal this.SumOfPendingChanges (pc:AbstractComponentChange[]) = 
        pc
        |> Array.fold (fun map c -> this.updateSumOfChanges map c) Map.empty 
        |> Map.toArray
        |> Array.map (fun tup -> snd tup)

    abstract member Initialize : unit
    abstract member Update : EntityComponentData * SystemChangeLog -> EntityComponentData * SystemChangeLog


type SystemManager() =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member this.RegisterSystems (sl:AbstractSystem[]) =
        _systems <- sl
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems (ecd:EntityComponentData) =
        this.ActiveAndInitialized |> Array.fold (fun d s -> s.Update d) (ecd, SystemChangeLog.empty)

