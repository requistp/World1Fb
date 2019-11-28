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


