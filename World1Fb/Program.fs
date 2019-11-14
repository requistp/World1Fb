open BuildNewWorld
open ControllerSystem
open EatingSystem
open EntitySystem
open FoodSystem
open FormSystem
open GameManager
open KillSystem
open MatingSystem
open MemorySystem
open MovementSystem
open PlantGrowthSystem
open SystemManager
open TerrainSystem
open VisionSystem
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

let g = new Game(setDisplay, wmr.UpdateEntity, wmr.MoveWindow, format)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 5)
    |> Array.append (MakeRabbits g.EntityManager 5)
    
let systems =
    [|
        ControllerSystem("ControllerSystem",g,true).Abstract
        EatingSystem("EatingSystem",g,true).Abstract
        EntitySystem("EntitySystem",g,true).Abstract
        FoodSystem("FoodSystem",g,true).Abstract
        //FormSystem(g,true,Middle).Abstract
        KillSystem("KillSystem",g,true).Abstract
        MatingSystem("MatingSystem",g,true).Abstract
        MemorySystem("MemorySystem",g,true).Abstract
        MovementSystem("MovementSystem",g,true).Abstract
        PlantGrowthSystem("PlantGrowthSystem",g,true).Abstract
        //TerrainSystem(g,true,Middle).Abstract
        VisionSystem("VisionSystem",g,true).Abstract
    |]

g.Start systems startingEntities ""

//g.Start systems Array.empty "Save_201911041343"

