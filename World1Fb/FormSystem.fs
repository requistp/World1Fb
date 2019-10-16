module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Initialize = 
        base.SetToInitialized

    override this.Update = 
        ()
