module MatingComponent
open CommonGenericFunctions
open ComponentEnums


type MatingComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        ChanceOfReproduction : float
        LastMatingAttempt : RoundNumber
        MatingStatus : MatingStatus
        Species : Species 
    }

let CanMate (m:MatingComponent) (round:RoundNumber) =
    (m.MatingStatus <> MatingStatus.Female_Pregnant) && (m.LastMatingAttempt = RoundNumber(0u) || m.LastMatingAttempt + m.Species.MaxMatingFrequency <= round)

let UpdateMating (m:MatingComponent) (chanceOfReproductionUpdate:float option) (matingStatusUpdate:MatingStatus option) (lastMatingAttemptUpdate:RoundNumber option) (speciesUpdate:Species option) =
    {
        m with
            ChanceOfReproduction = if chanceOfReproductionUpdate.IsNone then m.ChanceOfReproduction else chanceOfReproductionUpdate.Value
            LastMatingAttempt = if lastMatingAttemptUpdate.IsNone then m.LastMatingAttempt else lastMatingAttemptUpdate.Value
            MatingStatus = if matingStatusUpdate.IsNone || matingStatusUpdate.Value = Male then m.MatingStatus else matingStatusUpdate.Value
            Species = if speciesUpdate.IsNone then m.Species else speciesUpdate.Value
    }

