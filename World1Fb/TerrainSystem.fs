module TerrainSystem
open CommonGenericFunctions
open Components
open LocationTypes
open SystemManager
open TerrainComponent

type TerrainSystem(isActive:bool) =
    inherit AbstractSystem(isActive,true) 

    let MakeMap = //List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]
        let st = System.DateTime.Now
        let l = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))]) [0us..MapWidth - 1us]
        let l2 = List.P
        let et = System.DateTime.Now
        printfn "\nMakeMap start:%O, %i" st st.Millisecond
        printfn "MakeMap end  :%O, %i" et et.Millisecond
        printfn "Cost         :%i" (et.Subtract(st).Milliseconds)
        l
        //List.map 
        //List.collect
    override this.Initialize = 
        match this.IsActive with
        | false -> List.empty
        | true -> base.SetToInitialized
                  MakeMap |> List.collect (fun ct -> [EntityAddition [ct]])

    override this.Update ecm = 
        List.empty  
