module TerrainSystem
open CommonGenericFunctions
open Components
open LocationTypes
open GameManager
open TerrainComponent

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive) 

    override this.Initialize = 
        base.SetToInitialized
        ()
        //match this.IsActive with
        //| false -> List.empty
        //| true -> base.SetToInitialized
        //          map |> List.collect (fun ct -> [EntityAddition [ct]])

    override this.Update ecm = 
        () //List.empty  
