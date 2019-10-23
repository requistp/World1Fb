open AbstractSystem
open BuildNewWorld
open EatingSystem
open FoodSystem
open FormSystem
open GameManager
open KillSystem
open MovementSystem
open PlantGrowthSystem
open Renderer
open ScheduleSystem
open TerrainSystem

let g = new Game(RenderFrame)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 1)
    |> Array.append (MakeRabbits g.EntityManager 1)
    |> Array.rev 

let ss =
    [|
        EatingSystem(g,true).Abstract
        FoodSystem(g,true).Abstract
        FormSystem(g,true).Abstract
        KillSystem(g,true).Abstract
        MovementSystem(g,true).Abstract
        PlantGrowthSystem(g,true).Abstract
        ScheduleSystem(g,true).Abstract
        TerrainSystem(g,true).Abstract
    |]

g.Start ss startingEntities
