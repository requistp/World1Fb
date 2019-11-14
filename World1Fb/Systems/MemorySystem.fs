module MemorySystem
open GameManager
open SystemManager


type MemorySystem(description:string, game:Game, isActive:bool) =
    inherit AbstractSystem(description,isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    


    override me.Initialize = 
        //evm.RegisterListener me.ToString Event_ActionEat.ID             me.onEat
        base.SetToInitialized

    override me.Update round = 
        ()


        

