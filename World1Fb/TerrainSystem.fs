module TerrainSystem
open CommonGenericFunctions
open Components
open LocationTypes
open GameManager
open TerrainComponent

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 

    let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]

    override this.Initialize = 
        match this.IsActive with
        | false -> List.empty
        | true -> base.SetToInitialized
                  MakeMap |> List.collect (fun ct -> [EntityAddition [ct]])

    override this.Update ecm = 
        List.empty  
