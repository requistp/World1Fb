module MatingSystem
open Component
open ComponentEnums
open EventTypes
open GameManager
open SystemManager


type MatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member me.onActionMate (ge:GameEventTypes) =
        let e = ge.ToActionMate
        let mc = (e.EntityID|>enm.GetComponent MatingComponent.ID).ToMating
        let fc = (e.EntityID|>enm.GetComponent FormComponent.ID).ToForm
        let round = evm.GetRound()

        let checkMatingStatus =
            match mc.MatingStatus with
            | Female_Pregnant -> Error "Pregnant" 
            | Female -> Error "Female"
            | Male when mc.CanTryMating round -> Error "Too early to retry"
            | _ -> Ok None

        let eligible _ = 
            let mates = 
                fc.Location
                |> enm.GetEntitiesAtLocation  // here
                |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
                |> enm.TryGetComponentForEntities MatingComponent.ID // Can mate
                |> Array.map (fun c -> c.ToMating)
                |> Array.filter (fun m -> m.MatingStatus = Female && m.Species=mc.Species && m.CanTryMating round) // Not Pregnant Females Only & Same Species & Can Retry
                |> Array.sortByDescending (fun m -> m.ChanceOfReproduction)
            match mates with 
            | [||] -> Error "No eligible females present"
            | mc2 -> Ok mc2.[0]

        checkMatingStatus
        |> Result.bind eligible
        Ok None
        //|> Result.bind (fun zz -> Ok None)
            //match eligible with
            //| [||] -> Error "No eligible females present"
            //| _ ->
            //    Ok None
        
        //Result.bind

    override _.ToString = "MatingSystem"

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ActionMate.ID me.onActionMate
        base.SetToInitialized

    override me.Update = 
        ()


