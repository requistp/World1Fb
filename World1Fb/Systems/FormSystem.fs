module FormSystem
open GameManager
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    //Disabled

    override _.ToString = "FormSystem"

    override me.Update = 
        ()

