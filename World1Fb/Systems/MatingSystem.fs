﻿module MatingSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open MatingComponent
open SystemManager
open agent_Entities

type MatingSystem(description:string, isActive:bool, enm:agent_Entities, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    let MakeBaby momID round =
        let adjustComponents (c:Component) =
            match c with
            | Mating d -> 
                Mating { d with MatingStatus = if random.Next(2) = 0 then Male else Female }
            | Form d -> 
                Form { d with Born = round; Location = d.Location.Add { X=0; Y=0; Z=0 }} 
            | _ -> c    
        let newcts = 
            momID
            |> Entities.CopyEntity enm
            |> Array.Parallel.map (fun c -> adjustComponents c)
        evm.RaiseEvent (CreateEntity { Components = newcts })
        Ok (Some (sprintf "Born:%i" newcts.[0].EntityID))

    static let eligibleFemales (enm:agent_Entities) (mating:MatingComponent) round = 
        mating.EntityID 
        |> Entities.GetLocation enm
        |> Entities.GetEntitiesAtLocationWithComponent enm MatingComponentID (Some mating.EntityID)
        |> Array.Parallel.map (fun c -> c.ToMating)
        |> Array.filter (fun m -> m.Species = mating.Species && m.MatingStatus = Female && m.CanMate round) // Same Species & Non-Pregnant Females & Can Retry

    static member MateActionEnabled (enm:agent_Entities) (entityID:uint32) (round:uint32) =
        let m = (entityID|>enm.GetComponent MatingComponentID).ToMating
        match m.MatingStatus with
        | Male when m.CanMate round -> 
            (eligibleFemales enm m round).Length > 0
        | _ -> false

    member me.onActionMate round (ge:GameEventTypes) =
        let mc = (ge.EntityID|>enm.GetComponent MatingComponentID).ToMating
        
        let selectFemale = 
            let mates = 
                eligibleFemales enm mc round
                |> Array.sortByDescending (fun m -> m.ChanceOfReproduction)
            match mates with 
            | [||] -> Error "No eligible females present"
            | mc2 -> Ok mc2.[0]

        let getEligiblesDecision (mc2:MatingComponent) =
            // Add a better decision process here: can any mates with higher reproductive chance be seen? am I hungry?
            match random.Next(10) with
            | 0 -> Error "Female denied advances"
            | _ -> Ok mc2

        let tryMating (mc2:MatingComponent) =
            let chance = mc.ChanceOfReproduction * mc2.ChanceOfReproduction
            let rnd = random.NextDouble()
            enm.ReplaceComponent (Mating (mc.Update None None (Some round) None)) 
            match chance >= rnd with
            | false -> 
                enm.ReplaceComponent (Mating (mc2.Update None None (Some round) None))
                Error (sprintf "Reproduction failed (%f<%f)" chance rnd)
            | true ->
                evm.AddToSchedule round (ScheduleEvent ({ Schedule = RunOnce; Frequency = mc2.Species.Gestation }, Birth { MomID = mc2.EntityID; DadID = mc.EntityID }))
                enm.ReplaceComponent (Mating (mc2.Update None (Some Female_Pregnant) (Some round) None)) 
                Ok (Some (sprintf "Reproduction succeeded (%f >= %f)" chance rnd))

        selectFemale
        |> Result.bind getEligiblesDecision
        |> Result.bind tryMating

    member me.onBirth round (ge:GameEventTypes) =
        let e = ge.ToBirth
        let m = (e.MomID|>enm.GetComponent MatingComponentID).ToMating
        enm.ReplaceComponent (Mating (m.Update None (Some Female) (Some (round + m.Species.MaxMatingFrequency)) None)) // Change Mom to Non-Pregnant Female and add some extra time to before she can mate again
        MakeBaby e.MomID round

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionMate_ID (me.TrackTask me.onActionMate)
        evm.RegisterListener me.Description Event_Birth_ID      (me.TrackTask me.onBirth)
        base.SetToInitialized

    override me.Update round = 
        ()


// No longer necessary as I moved to the controller step
//let checkMatingStatus =
//    match mc.MatingStatus with
//    | Female_Pregnant -> Error "Pregnant" 
//    | Female -> Error "Female"
//    | Male when not (mc.CanMate round) -> Error "Too early to retry"
//    | _ -> Ok None

