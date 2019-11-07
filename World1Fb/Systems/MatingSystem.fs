module MatingSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open EventTypes
open GameManager
open MatingComponent
open SystemManager


type MatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    let MakeBaby momID =
        let adjustComponents (c:Component) =
            match c with
            | Mating d -> 
                Mating { d with MatingStatus = if random.Next(2) = 0 then Male else Female }
            | Form d -> 
                Form { d with Born = evm.GetRound(); Location = d.Location.Add { X=0; Y=0; Z=0 }} 
            | _ -> c    
        let newcts = 
            momID
            |> enm.CopyEntity
            |> Array.Parallel.map (fun c -> adjustComponents c)
        evm.ExecuteEvent (CreateEntity { Components = newcts })
        Ok (Some (sprintf "Born:%i" newcts.[0].EntityID))

    member me.onActionMate (ge:GameEventTypes) =
        let e = ge.ToActionMate
        let mc = (e.EntityID|>enm.GetComponent MatingComponentID).ToMating
        let fc = (e.EntityID|>enm.GetComponent FormComponentID).ToForm
        let round = evm.GetRound()

        let checkMatingStatus =
            match mc.MatingStatus with
            | Female_Pregnant -> Error "Pregnant" 
            | Female -> Error "Female"
            | Male when not (mc.CanMate round) -> Error "Too early to retry"
            | _ -> Ok None

        let findEligibleFemale _ = 
            let mates = 
                fc.Location
                |> enm.GetEntitiesAtLocation  // Here
                |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
                |> enm.TryGetComponentForEntities MatingComponentID // Can mate
                |> Array.map (fun c -> c.ToMating)
                |> Array.filter (fun m -> m.Species = mc.Species && m.MatingStatus = Female && m.CanMate round) // Same Species & Non-Pregnant Females & Can Retry
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
            enm.ReplaceComponent (Mating (mc.Update None None (Some round) None)) None |> ignore
            match chance >= rnd with
            | false -> 
                enm.ReplaceComponent (Mating (mc2.Update None None (Some round) None)) None |> ignore
                Error (sprintf "Reproduction failed (%f<%f)" chance rnd)
            | true ->
                evm.ScheduleEvent (ScheduleEvent ({ Schedule = RunOnce; Frequency = mc2.Species.Gestation }, Birth { MomID = mc2.EntityID; DadID = mc.EntityID }))
                enm.ReplaceComponent (Mating (mc2.Update None (Some Female_Pregnant) (Some round) None)) (Some (sprintf "Reproduction succeeded (%f >= %f)" chance rnd))

        checkMatingStatus
        |> Result.bind findEligibleFemale
        |> Result.bind getEligiblesDecision
        |> Result.bind tryMating

    member me.onBirth (ge:GameEventTypes) =
        let e = ge.ToBirth
        let m = (e.MomID|>enm.GetComponent MatingComponentID).ToMating
        enm.ReplaceComponent (Mating (m.Update None (Some Female) (Some (evm.GetRound() + m.Species.MaxMatingFrequency)) None)) None |> ignore // Change Mom to Non-Pregnant Female and add some extra time to before she can mate again
        MakeBaby e.MomID

    override _.ToString = "MatingSystem"

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ActionMate.ID me.onActionMate
        evm.RegisterListener me.ToString Event_Birth.ID      me.onBirth
        base.SetToInitialized

    override me.Update = 
        ()


