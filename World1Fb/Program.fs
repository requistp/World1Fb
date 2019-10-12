open BuildNewWorld
open CommonGenericFunctions
open EntityManager
open FormSystem
open GameManager
open LocationTypes
open MovementSystem
open Renderer
open SystemManager
open TerrainSystem

let g = new Game(RenderFrame)

let ss =
    [|
        FormSystem(g,true,MakeRabbits 1) :> AbstractSystem
        MovementSystem(g,true) :> AbstractSystem
        TerrainSystem(g,true,MakeMap) :> AbstractSystem
    |]

g.Start ss

