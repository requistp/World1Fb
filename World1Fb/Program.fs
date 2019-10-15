open BuildNewWorld
open CommonGenericFunctions
open EntityManager
open FoodSystem
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
        FoodSystem(g, true) :> AbstractSystem
        FormSystem(g, true, MakeRabbits g.EntityManager 1) :> AbstractSystem
        MovementSystem(g, true) :> AbstractSystem
        TerrainSystem(g, true, MakeMap g.EntityManager) :> AbstractSystem
    |]

g.Start ss

