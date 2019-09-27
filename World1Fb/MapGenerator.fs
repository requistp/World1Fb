module MapGenerator
//open Components
//open EntityComponentManager
//open LocationTypes

//let MakeMap =
//    let mutable ctl = List.empty:ComponentType list

//    let st = System.DateTime.Now

//    for x in [0us..MapWidth - 1us] do
//        for y in [0us..MapHeight - 1us] do
//            let t = Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))
//            ctl <- [t] @ ctl

//    let et = System.DateTime.Now
//    printfn "\nMap size:%i" ctl.Length
//    printfn "Map start:%O, %i" st st.Millisecond
//    printfn "Map   end:%O, %i" et et.Millisecond
//    printfn "Cost     :%i" (et.Subtract(st).Milliseconds)

//    ctl

//let TranslateMapToEntities (ecman:EntityComponentManager) (ctl:ComponentType list) =
//    let st = System.DateTime.Now

//    let newecm = ecman.CreateEntities_ViaComponentTypeList ctl

//    let et = System.DateTime.Now
//    printfn "\nTranslating map size:%i" ctl.Length
//    printfn "Translating map start:%O, %i" st st.Millisecond
//    printfn "Translating map   end:%O, %i" et et.Millisecond
//    printfn "Cost                 :%i" (et.Subtract(st).Milliseconds)

//    newecm
