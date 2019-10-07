module TerrainSystem
open CommonGenericFunctions
open Components
open LocationTypes
open GameManager
open TerrainComponent
open EntityComponentManager

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Initialize (ecd:EntityComponentData) = 
        base.SetToInitialized
        List.empty

    override this.Update (ecd:EntityComponentData) = 
        List.empty  
