module VisionSystem
open agent_Memories
open Component
open ComponentEnums
open EventTypes
open GameManager
open LocationTypes
open SystemManager
open VisionComponent


type VisionSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager
    //let mem = game.MemoryManager

    member private me.onLocationChanged (ge:GameEventTypes) = 
        let lc = ge.ToLocationChanged
        match lc.EntityID |> enm.TryGetComponent VisionComponentID with
        | None -> Ok (Some "No vision Component")
        | Some v ->
            let visionMap = LocationsWithinRange2D lc.Form.ToForm.Location v.ToVision.RangeTemplate
            let viewableMap = visionMap // Should handle occlusion 
            enm.ReplaceComponent (Vision (v.ToVision.Update (evm.GetRound()) None (Some viewableMap) (Some visionMap)))
            Ok None

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_LocationChanged_ID me.onLocationChanged
        base.SetToInitialized

    override _.ToString = "VisionSystem"

    override me.Update round = 
        ()//me.UpdateAllWithVision round


        
//member private me.UpdateAllWithVision round = 
//    VisionComponentID
//    |> enm.GetEntitiesWithComponent 
//    |> Array.Parallel.iter (fun e -> 
//        (e|>enm.GetComponent VisionComponentID).ToVision.RangeTemplate
//        |> LocationsWithinRange2D (e|>enm.GetComponent FormComponentID).ToForm.Location 
//        |> Array.Parallel.iter (fun l -> 
//            enm.GetEntitiesAtLocation l 
//            |> Array.Parallel.iter (fun oe -> 
//                mem.Record (Sight { EntityID = e; Time = round; Location = l; OtherEntityID=oe })
//                )))