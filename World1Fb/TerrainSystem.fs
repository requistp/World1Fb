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
        //match this.IsActive with
        //| false -> List.empty
        //| true -> base.SetToInitialized
        //          map |> List.collect (fun ct -> [EntityAddition [ct]])

    override this.Update (ecd:EntityComponentData) = 
        List.empty  
