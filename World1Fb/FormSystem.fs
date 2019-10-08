module FormSystem
open Components
open GameManager
open EntityComponentManager
open EventManager

type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
   
    override this.Initialize = 
        base.SetToInitialized
        List.empty

    override this.Update = 
        List.empty
