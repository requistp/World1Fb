module KillSystem
open EventTypes
open GameManager
open SystemManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onKillAllEaten (ge:GameEventTypes) =
        enm.RemoveEntity (ge.ToKillAllEaten.EateeID)

    override me.Initialize = 
        evm.RegisterListener "KillSystem" Event_KillAllEaten.ID me.onKillAllEaten
        base.SetToInitialized

    override me.Update = 
        ()

