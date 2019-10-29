module FormSystem
open AbstractSystem
open FormComponent
open EventTypes
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    override this.Update = 
        ()

