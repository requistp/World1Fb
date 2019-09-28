module TerrainSystem
open ChangeLogManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open LocationTypes
open System_Abstract
open TerrainComponent

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 

    let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]
   
    override this.Initialize = 
        base.SetToInitialized
        match this.IsActive with
        | true -> MakeMap |> List.collect (fun ct -> [EntityAddition [ct]])
        | false -> List.empty

    override this.Update ecm = 
        List.empty  
