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
open System

//let MakeMap = 
//    [|0us..MapWidth - 1us|] |> Array.collect (fun x -> for (y:uint16) in [|0us..MapHeight - 1us|] -> (Guid.NewGuid(), ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} }))) 

let MakeMap =    
    let AddTerrain x y = 
        let g = Guid.NewGuid()
        (g, [{ EntityID=g; Component=ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })}])

    let YLoop x = 
        [|0us..MapHeight-1us|]
        |> Array.map (fun y -> (AddTerrain x y))
  
    [|0us..MapWidth-1us|]
    |> Array.collect (fun x -> YLoop x)
    |> Map.ofArray