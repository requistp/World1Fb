open AbstractSystem
open BuildNewWorld
open CommonGenericFunctions
open EatingSystem
open EntityManager
open FoodSystem
open FormSystem
open GameManager
open LocationTypes
open KillSystem
open MovementSystem
open Renderer
open SystemManager
open TerrainSystem

let g = new Game(RenderFrame)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 5)
    |> Array.append (MakeRabbits g.EntityManager 3)
    |> Array.rev 

let ss =
    [|
        EatingSystem(g, true) :> AbstractSystem
        FoodSystem(g, true) :> AbstractSystem
        FormSystem(g, true) :> AbstractSystem
        KillSystem(g, true) :> AbstractSystem
        MovementSystem(g, true) :> AbstractSystem
        TerrainSystem(g, true) :> AbstractSystem
    |]

g.Start ss startingEntities

