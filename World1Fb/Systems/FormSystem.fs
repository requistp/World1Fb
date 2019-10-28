module FormSystem
open AbstractSystem
open FormComponent
open EventTypes
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Update = 
        ()

