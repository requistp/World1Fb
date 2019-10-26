module LocationEntityDictionary
open AbstractComponent
open EntityArray
open FormComponent
open LocationTypes

type LocationEntityDictionary() = 
    let locations =
        MapLocations |> Array.fold (fun (m:Map<LocationDataInt,EntityArray>) l -> m.Add(l,new EntityArray())) Map.empty

    member this.Add (form:FormComponent) =
        (locations.Item form.Location).Add form.EntityID
    member this.Add (cts:AbstractComponent[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentType = Component_Form)
        |> Array.iter (fun ct -> this.Add (ct:?>FormComponent))

    member this.Move (oldForm:FormComponent) (newForm:FormComponent) =
        this.Remove oldForm
        this.Add newForm

    member this.Remove (form:FormComponent) =
        (locations.Item form.Location).Remove form.EntityID
    member this.Remove (cts:AbstractComponent[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentType = Component_Form)
        |> Array.iter (fun ct -> this.Remove (ct:?>FormComponent))
        
    member this.List (location:LocationDataInt) =
        locations.Item(location).List
    member this.List () =
        locations |> Map.iter (fun k v -> printfn "%s | %i" (k.ToString()) v.List.Length)
        

