module BuildNewWorld
open CommonGenericFunctions
open LocationTypes
open Renderer
open GameManager
open FormSystem
open TerrainSystem
open Components
open TerrainComponent
open FormComponent
open EntityComponentManager

//let MakeMap ecm = 
//    [0us..MapWidth - 1us] 
//    |> List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })]) 

let MakeMap ecm = 
    let mutable newecm = ecm
    
    let AddTerrain x y = Entity.Create newecm [ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })]

    for x in [0us..MapWidth - 1us] do
        for y in [0us..MapHeight - 1us] do
            newecm <- AddTerrain x y
    newecm
