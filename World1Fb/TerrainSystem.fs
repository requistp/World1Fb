module TerrainSystem
open Components
open EntityComponentManager
open LocationTypes
open System_Abstract
open TerrainComponent

//let TranslateMapToEntities (ctl:ComponentType list) = 
//    let ecm = EntityComponentManager()
//    ecm.CreateEntities_ViaComponentTypeList ctl

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 

    let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]
   
    let TranslateMapToFrameChangeLog (ctl:ComponentType list) = 
        let mm2 = List.init MakeMap.Length (fun index -> index)
        printfn "%O" mm2.ToString
        let t = List.map2 (fun k v -> (k,v)) mm2 MakeMap
        //let mm2 = MakeMap |> List.map (fun x -> let mutable i = 0; (i,x))
        //let x = MakeMap |> Map.ofList MakeMap.It
        //entityAdditions:Map<string,ComponentType list>
        FrameChangeLog(Map.empty, Map.empty, List.empty)

    override this.Initialize = 
        this.IsInitialized <- true
        TranslateMapToFrameChangeLog MakeMap

    override this.Update ecm = 
        FrameChangeLog(Map.empty, Map.empty, List.empty)
