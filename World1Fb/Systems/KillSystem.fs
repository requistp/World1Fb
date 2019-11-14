module KillSystem
open EventTypes
open GameManager
open SystemManager


type KillSystem(description:string, game:Game, isActive:bool) =
    inherit AbstractSystem(description,isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onKillAllEaten round (ge:GameEventTypes) =
        enm.RemoveEntity (ge.ToKillAllEaten.EateeID)

    override me.Initialize = 
        evm.RegisterListener me.Description Event_KillAllEaten_ID (me.TrackTask me.onKillAllEaten)
        base.SetToInitialized

    override me.Update round = 
        ()

