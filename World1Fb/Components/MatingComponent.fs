module MatingComponent
open ComponentEnums


type MatingComponent = 
    { 
        EntityID:uint32
        ChanceOfReproduction:float
        LastMatingAttempt:uint32
        MatingStatus:MatingStatus
        Species:Species 
    } 
    with 

    member me.CanTryMating round =
        me.MatingStatus <> MatingStatus.Female_Pregnant && (me.LastMatingAttempt = 0u || me.LastMatingAttempt + me.Species.MaxMatingFrequency <= round)

    member me.Update (chanceOfReproductionUpdate:float option) (matingStatusUpdate:MatingStatus option) (lastMatingAttemptUpdate:uint32 option) (speciesUpdate:Species option) =
        {
            me with
                ChanceOfReproduction = if chanceOfReproductionUpdate.IsNone then me.ChanceOfReproduction else chanceOfReproductionUpdate.Value
                LastMatingAttempt = if lastMatingAttemptUpdate.IsNone then me.LastMatingAttempt else lastMatingAttemptUpdate.Value
                MatingStatus = if matingStatusUpdate.IsNone || matingStatusUpdate.Value = Male then me.MatingStatus else matingStatusUpdate.Value
                Species = if speciesUpdate.IsNone then me.Species else speciesUpdate.Value
        }

