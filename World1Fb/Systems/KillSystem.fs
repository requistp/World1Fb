module KillSystem
open EventTypes
open GameManager
open SystemManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onKillAllEaten round (ge:GameEventTypes) =
        enm.RemoveEntity (ge.ToKillAllEaten.EateeID)

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_KillAllEaten_ID me.onKillAllEaten
        base.SetToInitialized

    override _.ToString = "KillSystem"

    override me.Update round = 
        ()

