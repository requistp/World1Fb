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
    member me.CanMate (round:RoundNumber) =
        (me.MatingStatus <> MatingStatus.Female_Pregnant) && (me.LastMatingAttempt = RoundNumber(0u) || me.LastMatingAttempt + me.Species.MaxMatingFrequency <= round)

    member me.Update (chanceOfReproductionUpdate:float option) (matingStatusUpdate:MatingStatus option) (lastMatingAttemptUpdate:RoundNumber option) (speciesUpdate:Species option) =
        {
            me with
                ChanceOfReproduction = if chanceOfReproductionUpdate.IsNone then me.ChanceOfReproduction else chanceOfReproductionUpdate.Value
                LastMatingAttempt = if lastMatingAttemptUpdate.IsNone then me.LastMatingAttempt else lastMatingAttemptUpdate.Value
                MatingStatus = if matingStatusUpdate.IsNone || matingStatusUpdate.Value = Male then me.MatingStatus else matingStatusUpdate.Value
                Species = if speciesUpdate.IsNone then me.Species else speciesUpdate.Value
        }

