module TerrainSystem
open Components
open EntityComponentManager
open LocationTypes
open SystemManager

let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]

let TranslateMapToEntities (ecman:EntityComponentManager) (ctl:ComponentType list) = ecman.CreateEntities_ViaComponentTypeList ctl

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 
   
    override this.Initialize ecm = 
        this.IsInitialized <- true
        TranslateMapToEntities ecm MakeMap

    override this.Update dt ecm = 
        ecm
