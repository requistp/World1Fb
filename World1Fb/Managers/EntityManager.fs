module EntityManager
open AbstractComponent
open CommonGenericFunctions
open ComponentEntityAgent
open EntityComponentAgent
open FormComponent
open LocationEntityAgent
open LocationTypes

type EntityManager() =

    let components = new ComponentEntityAgent()
    let entities = new EntityComponentAgent() 
    let locations = new LocationEntityAgent()

    member this.Components_Next = components
    member this.EntityID_Max = entities.GetMaxID
    member this.EntityID_New = entities.GetNewID
    member this.Locations_Next = locations

    member this.CopyEntity (oldeid:uint32) (neweid:uint32) =
        oldeid
        |> entities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    member internal this.CreateEntity (cts:AbstractComponent[]) = 
        entities.CreateEntity cts
        components.Add cts
        locations.Add cts
        Ok None

    member this.Exists (eid:uint32) = 
        entities.Exists eid

    member this.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T =
        (entities.GetComponents eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T
    
    member this.GetLocation (location:LocationDataInt) =
        locations.Get location

    member this.GetEntitiesWithComponent ct =
        components.Get ct

    member this.HasAllComponents (cts:'T[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> entities.GetComponents eid |> Array.exists (fun ec -> ec.GetType() = ct))

    member this.RemoveEntity (eid:uint32) =
        components.Remove (entities.GetComponents eid)
        locations.Remove (entities.GetComponents eid)
        entities.RemoveEntity eid
        Ok None

    member this.ReplaceComponent (ac:AbstractComponent) (changes:string option) =
        if ac.ComponentType = Component_Form then 
            locations.Move (this.GetComponent<FormComponent> ac.EntityID) (ac :?> FormComponent)
        entities.ReplaceComponent ac
        Ok changes

    member this.SetToNext() : Result<string option,string> =
        //_components_Current.Init (_components_Next.GetMap())
        //_locations_Current.Init (_locations_Next.GetMap())
        Ok None

    member this.TryGet (eid:uint32) =
        entities.Exists eid |> TrueSomeFalseNone (entities.GetComponents eid)

    member this.TryGetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : Option<'T> = 
        match this.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)

    member this.TryGetComponentForEntities<'T when 'T:>AbstractComponent> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> this.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)


    member this.CurrentToString() =
        //_locations_Current.Print()
        //_components_Current.Print
        //_entities_Current.Print
        ()


