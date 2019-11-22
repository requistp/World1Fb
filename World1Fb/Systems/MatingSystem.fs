module MatingSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open EntityExtensions
open EventManager
open EventTypes
open MatingComponent
open SystemManager
open EntityManager

let private EligibleFemales (enm:EntityManager) (mating:MatingComponent) round = 
    mating.EntityID 
    |> EntityExt.GetLocation enm None
    |> EntityExt.GetEntitiesAtLocationWithComponent enm None MatingComponentID (Some mating.EntityID)
    |> Array.Parallel.map ToMating
    |> Array.filter (fun m -> m.Species = mating.Species && m.MatingStatus = Female && m.CanMate round) // Same Species & Non-Pregnant Females & Can Retry

let MateActionEnabled (enm:EntityManager) (entityID:EntityID) (round:RoundNumber) =
    let (Mating m) = enm.GetComponent None MatingComponentID entityID
    match m.MatingStatus with
    | Male when m.CanMate round -> 
        (EligibleFemales enm m round).Length > 0
    | _ -> false

type MatingSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    let makeBaby momID round =
        let adjustComponents (c:Component) =
            match c with
            | Mating d -> 
                Mating { d with MatingStatus = if random.Next(2) = 0 then Male else Female }
            | Form d -> 
                Form { d with Born = round; Location = d.Location.Add { X=0; Y=0; Z=0 }} 
            | _ -> c
        let newcts = 
            momID
            |> EntityExt.CopyEntity enm
            |> Array.Parallel.map adjustComponents
        evm.RaiseEvent (CreateEntity { Components = newcts })
        Ok (Some (sprintf "Born:%i" newcts.[0].EntityID))

    member me.onActionMate round (ge:GameEventTypes) =
        let (Mating mc) = enm.GetComponent None MatingComponentID ge.EntityID
        
        let selectFemale = 
            let mates = 
                EligibleFemales enm mc round
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
            enm.UpdateComponent round (Mating (mc.Update None None (Some round) None)) 
            match chance >= rnd with
            | false -> 
                enm.UpdateComponent round (Mating (mc2.Update None None (Some round) None))
                Error (sprintf "Reproduction failed (%f<%f)" chance rnd)
            | true ->
                evm.AddToSchedule (ScheduleEvent ({ Schedule = RunOnce; Frequency = mc2.Species.Gestation }, Birth { MomID = mc2.EntityID; DadID = mc.EntityID }))
                enm.UpdateComponent round (Mating (mc2.Update None (Some Female_Pregnant) (Some round) None)) 
                Ok (Some (sprintf "Reproduction succeeded (%f >= %f)" chance rnd))

        selectFemale
        |> Result.bind getEligiblesDecision
        |> Result.bind tryMating

    member me.onBirth round (ge:GameEventTypes) =
        let (Birth e) = ge
        let (Mating m) = enm.GetComponent None MatingComponentID e.MomID
        enm.UpdateComponent round (Mating (m.Update None (Some Female) (Some (round + m.Species.MaxMatingFrequency)) None)) // Change Mom to Non-Pregnant Female and add some extra time to before she can mate again
        makeBaby e.MomID round

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionMate_ID (me.TrackTask me.onActionMate)
        evm.RegisterListener me.Description Event_Birth_ID      (me.TrackTask me.onBirth)
        base.SetToInitialized

    override me.Update round = 
        ()

