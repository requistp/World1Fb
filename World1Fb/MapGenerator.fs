module MapGenerator
open Components
open EntityComponentManager
open LocationTypes

let MakeMap =
    let mutable ecm = EntityComponentManager()

    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            let t = Terrain(TerrainComponent(Dirt, LocationDataInt(x,y)))
            ecm <- ecm.CreateEntity(t)
    ecm
