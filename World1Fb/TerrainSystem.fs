module TerrainSystem
open Components
open EntityComponentManager
open LocationTypes
open System_Abstract
open TerrainComponent

let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]

let TranslateMapToEntities (ctl:ComponentType list) = 
    let ecm = EntityComponentManager()
    ecm.CreateEntities_ViaComponentTypeList ctl

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 
   
    override this.Initialize = 
        this.IsInitialized <- true
        TranslateMapToEntities MakeMap

    override this.Update ecm = 
        ecm
