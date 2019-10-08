module MovementSystem
open MovementComponent
open GameManager
open EntityComponentManager
open AbstractComponent
open EventManager
open System
open Components
open FormComponent

type SystemChanges = {
    ECD : EntityComponentData
    Changes : AbstractComponent_Change[]
    SumOfChanges : AbstractComponent_ChangeSum[]
    }

type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let mutable _pendingChanges = Array.empty<MovementComponent_Change>

    let onMovementKeyPressed (e:GameEvent) = 
        let (KeyPressed_Movement m) = e
        match Entity.AllWithComponent game.ECD ComponentID_Controller with
        | [] -> ()
        | l -> _pendingChanges <- Array.append _pendingChanges [| MovementComponent_Change(l.Head, m, m.X_change, m.Y_change) |]

    let updateSumOfChanges (map:Map<uint32,MovementComponent_ChangeSum>) (c:MovementComponent_Change) =
        let eid = c.EntityID
        match map.ContainsKey(eid) with
        | false -> map.Add(eid, MovementComponent_ChangeSum(eid, c.X, c.Y))
        | true -> let i = map.Item(eid)
                  map.Remove(eid).Add(eid, MovementComponent_ChangeSum(eid, i.X + c.X, i.Y + c.Y))

    let sumOfPendingChanges = 
        _pendingChanges
        |> Array.fold (fun map c -> updateSumOfChanges map c) Map.empty 
        |> Map.toArray
        |> Array.map (fun tup -> snd tup)

    let applyChange (ecd:EntityComponentData) (c:MovementComponent_ChangeSum) =
        match Entity.TryGetComponent ecd ComponentID_Form c.EntityID with
        | None -> ecd
        | Some ac -> let (Form oldc) = ac
                     let newc = { IsPassable = oldc.IsPassable; Name = oldc.Name; Symbol = oldc.Symbol; Location = oldc.Location.Add c.X c.Y }:FormComponent
                     //Entity.ReplaceComponent ecd c.EntityID ComponentID_Form newc
                     ecd

    let applyChangesToEntities (ecd:EntityComponentData) (s:MovementComponent_ChangeSum[]) = 
        s |> Array.fold (fun ecd c -> applyChange ecd c) ecd

    override _.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener GameEventID_KeyPressed_Movement onMovementKeyPressed
        List.empty

    override _.Update = 
        printfn "Movement changes to process: %i" _pendingChanges.Length

        let c = _pendingChanges
        let s = sumOfPendingChanges
        
        _pendingChanges <- Array.empty
        
        let newecd = applyChangesToEntities game.ECD s
        
        //{
        //    ECD : newecd
        //    Changes : c |> Array.map (fun x -> x :> AbstractComponent_Change)
        //    SumOfChanges : s |> Array.map (fun x -> x :> AbstractComponent_ChangeSum)
        //}
        List.empty
