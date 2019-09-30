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

let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })]) [0us..MapWidth - 1us]

