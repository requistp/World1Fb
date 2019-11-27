module MatingSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open EntityExtensions
open EventManager
open EventTypes
open FormComponent
open LocationTypes
open MatingComponent
open SystemManager
open EntityManager

let private EligibleFemales (enm:EntityManager) (mating:MatingComponent) round = 
    mating.EntityID 
    |> EntityExt.GetLocation enm
    |> EntityExt.GetEntitiesAtLocationWithComponent enm MatingComponentType (Some mating.EntityID)
    |> Array.map ToMating
    |> Array.filter (fun m -> m.Species = mating.Species && m.MatingStatus = Female && CanMate m round) // Same Species & Non-Pregnant Females & Can Retry

let MateActionEnabled (enm:EntityManager) (entityID:EntityID) (round:RoundNumber) =
    let (Mating m) = enm.GetComponent MatingComponentType entityID
    match m.MatingStatus with
    | Male when CanMate m round -> 
        (EligibleFemales enm m round).Length > 0
    | _ -> false

type MatingSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member me.onActionMate round (Action_Mate mc:GameEventData) =
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
            enm.UpdateComponent (Mating(MatingComponent(mc.ID, mc.EntityID, mc.ChanceOfReproduction, round, mc.MatingStatus, mc.Species))) // (UpdateMating mc None None (Some round) None)) 
            match chance >= rnd with
            | false -> 
                enm.UpdateComponent (Mating(MatingComponent(mc2.ID, mc2.EntityID, mc2.ChanceOfReproduction, round, mc2.MatingStatus, mc2.Species)))
                Error (sprintf "Reproduction failed (%f<%f)" chance rnd)
            | true ->
                evm.AddToSchedule { ScheduleType = RunOnce; Frequency = mc2.Species.Gestation; GameEvent = Birth (mc2,mc) }
                enm.UpdateComponent (Mating(MatingComponent(mc2.ID, mc2.EntityID, mc2.ChanceOfReproduction, round, Female_Pregnant, mc2.Species)))
                Ok (Some (sprintf "Reproduction succeeded (%f >= %f)" chance rnd))

        selectFemale
        |> Result.bind getEligiblesDecision
        |> Result.bind tryMating

    member me.onBirth round (Birth (mom,_):GameEventData) =
        enm.UpdateComponent (Mating(MatingComponent(mom.ID, mom.EntityID, mom.ChanceOfReproduction, round + mom.Species.MaxMatingFrequency, Female, mom.Species))) // (Mating (UpdateMating mom None (Some Female) (Some (round + mom.Species.MaxMatingFrequency)) None)) // Change Mom to Non-Pregnant Female and add some extra time to before she can mate again
        let adjustComponents (c:Component) =
            match c with
            | Mating d -> 
                Mating(MatingComponent(d.ID, d.EntityID, d.ChanceOfReproduction, d.LastMatingAttempt, (if random.Next(2) = 0 then Male else Female), d.Species))
                //{ d with MatingStatus = if random.Next(2) = 0 then Male else Female }
            | Form d -> 
                Form(FormComponent(d.ID, d.EntityID, d.Born, d.CanSeePast, d.IsPassable, d.Location, d.Name, d.Symbol))
                //Form { d with Location = d.Location + { X = 0; Y = 0; Z = 0 }} 
            | _ -> c
        let newcts = 
            mom.EntityID
            |> EntityExt.CopyEntity enm round
            |> Array.map adjustComponents
        evm.RaiseEvent (CreateEntity newcts)
        Ok (Some (sprintf "Born:%i" (GetComponentEntityID newcts.[0]).ToUint32))

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionMate (me.TrackTask me.onActionMate)
        evm.RegisterListener me.Description Event_Birth      (me.TrackTask me.onBirth)
        base.SetToInitialized

    override me.Update round = 
        ()

