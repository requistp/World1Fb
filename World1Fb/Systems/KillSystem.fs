module KillSystem
open AbstractSystem
open EntityManager
open FormComponent
open EventTypes
open GameManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onKillAllEaten (enm:EntityManager) (ge:EventData_Generic) =
        enm.RemoveEntity (ge.EntityID)

    override this.Initialize = 
        game.EventManager.RegisterListener "KillSystem" Kill_AllEaten this.onKillAllEaten
        base.SetToInitialized

    override this.Update = 
        ()

