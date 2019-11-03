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
open TerrainSystem
open ConsoleV1
open WorldMapRenderer


let wmr = new WorldMapRenderer()

let windows =
    [|
        "World Map",""
        "Game Events List",""
    |]
let windowMan = new WindowManager(windows)

let setContent = windowMan.SetContent
let setDisplay = windowMan.SetDisplay
let display = windowMan.Display

let g = new Game(RenderFrame, setContent, setDisplay, display, wmr.Update, wmr.MoveWindow)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 1)
    |> Array.append (MakeRabbits g.EntityManager 1)
    
let ss =
    [|
        EatingSystem(g,true).Abstract
        FoodSystem(g,true).Abstract
        FormSystem(g,true).Abstract
        KillSystem(g,true).Abstract
        MovementSystem(g,true).Abstract
        PlantGrowthSystem(g,true).Abstract
        TerrainSystem(g,true).Abstract
    |]

g.Start ss startingEntities




