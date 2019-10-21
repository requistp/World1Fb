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
open PlantGrowthSystem
open Renderer
open ScheduleSystem
open SystemManager
open TerrainSystem

let g = new Game(RenderFrame)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 1)
    |> Array.append (MakeRabbits g.EntityManager 1)
    |> Array.rev 

let ss =
    [|
        EatingSystem(g, true) :> AbstractSystem
        FoodSystem(g, true) :> AbstractSystem
        FormSystem(g, true) :> AbstractSystem
        KillSystem(g, true) :> AbstractSystem
        MovementSystem(g, true) :> AbstractSystem
        PlantGrowthSystem(g, true) :> AbstractSystem
        ScheduleSystem(g, true) :> AbstractSystem
        TerrainSystem(g, true) :> AbstractSystem
    |]

g.Start ss startingEntities

