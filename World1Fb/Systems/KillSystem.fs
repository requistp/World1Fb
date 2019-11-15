module KillSystem
open EntityManager
open EventManager   
open EventTypes
open SystemManager


type KillSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onKillAllEaten round (ge:GameEventTypes) =
        enm.AgentEntities.RemoveEntity ge.ToKillAllEaten.EateeID

    override me.Initialize = 
        evm.RegisterListener me.Description Event_KillAllEaten_ID (me.TrackTask me.onKillAllEaten)
        base.SetToInitialized

    override me.Update round = 
        ()

