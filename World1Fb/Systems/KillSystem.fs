module KillSystem
open AbstractSystem
open EntityManager
open FormComponent
open EventTypes
open GameManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private this.onKillAllEaten (ge:EventData_Generic) =
        enm.RemoveEntity (ge.EntityID)

    override this.Initialize = 
        evm.RegisterListener "KillSystem" Kill_AllEaten this.onKillAllEaten
        base.SetToInitialized

    override this.Update = 
        ()

