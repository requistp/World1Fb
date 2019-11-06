open BuildNewWorld
open EatingSystem
open EntitySystem
open FoodSystem
open FormSystem
open GameManager
open KillSystem
open MatingSystem
open MovementSystem
open PlantGrowthSystem
open TerrainSystem
open ConsoleV1
open WorldMapRenderer

let format = LoadAndSave.SaveGameFormats.XML

let wmr = new WorldMapRenderer()

let windows =
    [|
        "World Map",""
        "Game Events List",""
    |]
let windowMan = new WindowManager(windows)

let setDisplay = windowMan.SetDisplay

let g = new Game(setDisplay, wmr.Update, wmr.MoveWindow, format)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 1)
    |> Array.append (MakeRabbits g.EntityManager 10)
    
let systems =
    [|
        EatingSystem(g,true).Abstract
        EntitySystem(g,true).Abstract
        FoodSystem(g,true).Abstract
        //FormSystem(g,true).Abstract
        KillSystem(g,true).Abstract
        MatingSystem(g,true).Abstract
        MovementSystem(g,true).Abstract
        PlantGrowthSystem(g,true).Abstract
        //TerrainSystem(g,true).Abstract
    |]

g.Start systems startingEntities ""

//g.Start systems Array.empty "Save_201911041343"

