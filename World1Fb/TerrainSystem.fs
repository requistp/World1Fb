module TerrainSystem
open AbstractComponent
open CommonGenericFunctions
open EntityManager
open EventManager
open GameEvents
open GameManager
open LocationTypes
open SystemManager
open TerrainComponent

type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Initialize = 
        base.SetToInitialized
        
