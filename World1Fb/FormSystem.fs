module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open EntityComponentManager
open EventManager
open LocationTypes
open SystemManager

type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    let mutable _pendingChanges = Array.empty<FormComponent_Change>
   
    let onMovement (ge:AbstractGameEvent) =
        let m = ge :?> GameEvent_Movement
        _pendingChanges <- Array.append _pendingChanges [|FormComponent_Change(m.EntityID, None, None, { X=m.Direction.X_change; Y=m.Direction.Y_change })|]
        
    let updateSumOfChanges (map:Map<uint32,FormComponent_Change>) (c:FormComponent_Change) =
        match map.ContainsKey(c.EntityID) with
        | false -> map.Add(c.EntityID,c)
        | true -> let i = map.Item(c.EntityID)
                  map.Remove(c.EntityID).Add(c.EntityID,i.AddChange(c))
    
    let sumOfPendingChanges (pc:FormComponent_Change[]) = 
        pc
        |> Array.fold (fun map c -> updateSumOfChanges map c) Map.empty 
        |> Map.toArray
        |> Array.map (fun tup -> snd tup)

    let applyChanges (ecd:EntityComponentData) (c:FormComponent_Change) =
        match c.EntityID |> Entity.TryGetComponent ecd.Entities c.ComponentType with
        | None -> ecd
        | Some a -> a :?> FormComponent
                    |> c.AddChange
                    |> Entity.ReplaceComponent ecd c.EntityID

    override _.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener Movement onMovement

    override this.Update (ecd:EntityComponentData, scl:SystemChangeLog)= 
        let c = _pendingChanges
        _pendingChanges <- Array.empty

        let s = sumOfPendingChanges c
        
        let newecd = s |> Array.fold (fun e c -> applyChanges e c) ecd

        (newecd, scl.Add(SystemChangeLog.New(c,s)))  
        

        
//let applyChange (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum) =
//    match sumOfChanges.EntityID |> Entity.TryGetComponent ecd.Entities Form with
//    | None -> ecd
//    | Some ac -> let oldc = ac :?> FormComponent
//                 FormComponent(oldc.IsPassable, oldc.Name, oldc.Symbol, oldc.Location.Add sumOfChanges.X sumOfChanges.Y)
//                 |> Entity.ReplaceComponent ecd sumOfChanges.EntityID
        
//let applyChangesToEntities (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum[]) = 
//    sumOfChanges |> Array.fold (fun ecd c -> applyChange ecd c) ecd
        
        

        