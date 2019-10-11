open BuildNewWorld
open CommonGenericFunctions
open EntityComponentManager
open FormSystem
open GameManager
open LocationTypes
open MovementSystem
open Renderer
open SystemManager
open TerrainSystem

let g = new Game(RenderFrame)


[|
    FormSystem(g, true, MakeRabbits 1) :> AbstractSystem
    MovementSystem(g, true) :> AbstractSystem
    TerrainSystem(g, true, MakeMap) :> AbstractSystem
|]
|> g.SystemManager.Initialize

g.Start 

