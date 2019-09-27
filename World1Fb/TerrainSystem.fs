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
   
    let TranslateMapToFrameChangeLog (ctl:ComponentType list) = 
        clm.AddEntities_ViaComponentTypeList ctl

    override this.Initialize (clm:ChangeLogManager) = 
        match TranslateMapToFrameChangeLog clm MakeMap with
        | Error e -> Error e
        | Ok s -> this.IsInitialized <- true
                  Ok s

    override this.Update ecm = 
        ChangeLog(Map.empty, Map.empty, List.empty)
