﻿module MovementSystem
open AbstractComponent
open ControllerComponent
open EntityComponentManager
open EventManager
open FormComponent
open GameManager
open MovementComponent
open System

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

    let applyChange (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum) =
        match sumOfChanges.EntityID |> Entity.TryGetComponent ecd.Entities ComponentID_Form with
        | None -> ecd
        | Some ac -> let oldc = ac :?> FormComponent
                     FormComponent(oldc.IsPassable, oldc.Name, oldc.Symbol, oldc.Location.Add sumOfChanges.X sumOfChanges.Y)
                     |> Entity.ReplaceComponent ecd sumOfChanges.EntityID

    let applyChangesToEntities (ecd:EntityComponentData) (sumOfChanges:MovementComponent_ChangeSum[]) = 
        sumOfChanges |> Array.fold (fun ecd c -> applyChange ecd c) ecd

    override _.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener GameEventID_KeyPressed_Movement onMovementKeyPressed
        ()

    override _.Update = 
        printfn "Movement changes to process: %i" _pendingChanges.Length

        let c = _pendingChanges
        let s = sumOfPendingChanges
        
        _pendingChanges <- Array.empty
        
        let newecd = applyChangesToEntities game.ECD s
        
        {
            ECD = newecd
            Changes = c // |> Array.map (fun x -> x :> AbstractComponent_Change)
            SumOfChanges = s // |> Array.map (fun x -> x :> AbstractComponent_ChangeSum)
        }
