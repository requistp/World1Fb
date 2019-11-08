module MemorySystem
open GameManager
open SystemManager


type MemorySystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    


    override me.Initialize = 
        //evm.RegisterListener me.ToString Event_ActionEat.ID             me.onEat
        base.SetToInitialized

    override _.ToString = "MemorySystem"

    override me.Update round = 
        ()


        

