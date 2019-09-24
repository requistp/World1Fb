module MapGenerator
open Components
open EntityComponentManager
open LocationTypes

let MakeMap (ecmap:EntityComponentMap) =
    let mutable newecm = ecmap

    let st = System.DateTime.Now

    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            let t = Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))
            let ec = EntityComponent(newecm.MaxEntityID+1u, t)
            newecm <- newecm.Add([ec])

    let et = System.DateTime.Now
    printfn "\nMap start:%O, %i" st st.Millisecond
    printfn "\nMap end  :%O, %i" et et.Millisecond

    newecm
