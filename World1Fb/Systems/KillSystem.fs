module KillSystem
open EventManager   
open EventTypes
open SystemManager
open EntityManager


type KillSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onKillAllEaten round (Kill_AllEaten (eat,food):GameEventData) =
        enm.RemoveEntity round food.EntityID

    override me.Initialize = 
        evm.RegisterListener me.Description Event_KillAllEaten (me.TrackTask me.onKillAllEaten)
        base.SetToInitialized

    override me.Update round = 
        ()

