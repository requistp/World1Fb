open BuildNewWorld
open CommonGenericFunctions
open EatingSystem
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

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 1)
    |> Array.append (MakeRabbits g.EntityManager 1)

let ss =
    [|
        //EatingSystem(g, true) :> AbstractSystem
        //FoodSystem(g, true) :> AbstractSystem
        FormSystem(g, true) :> AbstractSystem
        MovementSystem(g, true) :> AbstractSystem
        TerrainSystem(g, true) :> AbstractSystem
    |]

g.Start ss startingEntities

