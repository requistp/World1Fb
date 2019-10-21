module KillSystem
open AbstractSystem
open EntityDictionary
open FormComponent
open EventTypes
open GameManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onKillAllEaten (next:NextEntityDictionary) (ge:EventData_Generic) =
        next.RemoveEntity (ge.EntityID)

    override this.Initialize = 
        game.EventManager.RegisterListener Kill_AllEaten this.onKillAllEaten
        base.SetToInitialized

