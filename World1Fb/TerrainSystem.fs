module TerrainSystem
open CommonGenericFunctions
open AbstractComponent
open LocationTypes
open GameManager
open TerrainComponent
open EntityComponentManager
open EventManager

type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Initialize = 
        base.SetToInitialized
        ()

    override this.Update (ecd:EntityComponentData, scl:SystemChangeLog)= 
        (ecd,scl) 

