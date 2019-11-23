module KillSystem
open EventManager   
open EventTypes
open SystemManager
open EntityManager


type KillSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onKillAllEaten round (Kill_AllEaten (eat,food):GameEventTypes) =
        enm.RemoveEntity round food.EntityID

    override me.Initialize = 
        evm.RegisterListener me.Description Event_KillAllEaten_ID (me.TrackTask me.onKillAllEaten)
        base.SetToInitialized

    override me.Update round = 
        ()

