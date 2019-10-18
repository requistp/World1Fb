module KillSystem
open AbstractSystem
open EntityDictionary
open FormComponent
open GameEvents
open GameManager


type KillSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onKillAllEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Kill_AllEaten        
        next.RemoveEntity (e.EntityID)

    override this.Initialize = 
        game.EventManager.RegisterListener Kill_AllEaten this.onKillAllEaten
        base.SetToInitialized

