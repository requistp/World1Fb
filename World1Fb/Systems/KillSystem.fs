module KillSystem
open EntityManager
open EventManager   
open EventTypes
open SystemManager
open agent_Entities

type KillSystem(description:string, isActive:bool, enm:agent_Entities, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 

    member private me.onKillAllEaten round (ge:GameEventTypes) =
        enm.RemoveEntity ge.ToKillAllEaten.EateeID

    override me.Initialize = 
        evm.RegisterListener me.Description Event_KillAllEaten_ID (me.TrackTask me.onKillAllEaten)
        base.SetToInitialized

    override me.Update round = 
        ()

