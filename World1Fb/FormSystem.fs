module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open EntityComponentManager
open EventManager
open LocationTypes
open System

type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    let mutable _pendingChanges = Array.empty<FormComponent_Change>
   
    let onMovement (ge:AbstractGameEvent) =
        let m = ge :?> GameEvent_Movement
        _pendingChanges <- Array.append _pendingChanges [|FormComponent_Change(m.EntityID, None, None, { X=m.Direction.X_change; Y=m.Direction.Y_change })|]
        ()

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

    override this.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener Movement onMovement
        ()

    override this.CompilePendingChanges =
        printfn "Form changes to process: %i" _pendingChanges.Length
        let c = _pendingChanges
        _pendingChanges <- Array.empty
        let s = sumOfPendingChanges c
        {
            ChangeLog = c // |> Array.map (fun x -> x :> AbstractComponent_Change)
            SumOfChanges = s // |> Array.map (fun x -> x :> AbstractComponent_ChangeSum)
        }    

    override this.Update = 
        ()    
        
//let updateSumOfChanges (map:Map<uint32,MovementComponent_ChangeSum>) (c:MovementComponent_Change) =
//    let eid = c.EntityID
//    match map.ContainsKey(eid) with
//    | false -> map.Add(eid, MovementComponent_ChangeSum(eid, c.X, c.Y))
//    | true -> let i = map.Item(eid)
//              map.Remove(eid).Add(eid, MovementComponent_ChangeSum(eid, i.X + c.X, i.Y + c.Y))
        
//let sumOfPendingChanges = 
//    _pendingChanges
//    |> Array.fold (fun map c -> updateSumOfChanges map c) Map.empty 
//    |> Map.toArray
//    |> Array.map (fun tup -> snd tup)
        
//let applyChange (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum) =
//    match sumOfChanges.EntityID |> Entity.TryGetComponent ecd.Entities Form with
//    | None -> ecd
//    | Some ac -> let oldc = ac :?> FormComponent
//                 FormComponent(oldc.IsPassable, oldc.Name, oldc.Symbol, oldc.Location.Add sumOfChanges.X sumOfChanges.Y)
//                 |> Entity.ReplaceComponent ecd sumOfChanges.EntityID
        
//let applyChangesToEntities (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum[]) = 
//    sumOfChanges |> Array.fold (fun ecd c -> applyChange ecd c) ecd
        
        

        