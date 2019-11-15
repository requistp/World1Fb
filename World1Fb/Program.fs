open BuildNewWorld
open ControllerSystem
open EatingSystem
open EntitySystem
open FoodSystem
open GameManager
open KillSystem
open MatingSystem
open MovementSystem
open PlantGrowthSystem
open SystemManager
open VisionSystem
open ConsoleV1
open WorldMapRenderer


let format = LoadAndSave.SaveGameFormats.Binary

let wmr = new WorldMapRenderer()

let g = new Game(wmr.UpdateEntity, format)

let startingEntities = 
    MakeMap g.EntityManager
    |> Array.append (MakeGrasses g.EntityManager 5)
    |> Array.append (MakeRabbits g.EntityManager 5)
    
let systems =
    [|
        ControllerSystem("Controller System", true, g.EntityManager, g.EventManager).Abstract
        EatingSystem("Eating System", true, g.EntityManager, g.EventManager).Abstract
        EntitySystem("Entity System", true, g.EntityManager, g.EventManager).Abstract
        FoodSystem("Food System", true, g.EntityManager, g.EventManager).Abstract
        KillSystem("Kill System", true, g.EntityManager, g.EventManager).Abstract
        MatingSystem("Mating System", true, g.EntityManager, g.EventManager).Abstract
        MovementSystem("Movement System", true, g.EntityManager, g.EventManager).Abstract
        PlantGrowthSystem("Plant Growth System", true, g.EntityManager, g.EventManager).Abstract
        VisionSystem("Vision System", true, g.EntityManager, g.EventManager).Abstract
    |]

g.Start systems startingEntities ""

//g.Start systems Array.empty "Save_201911041343"



