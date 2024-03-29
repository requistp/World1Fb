﻿open BuildNewWorld
open CommonGenericFunctions
open ControllerSystem
open EatingSystem
open EntitySystem
open FoodSystem
open GameManager
open KillSystem
open MatingSystem
open MovementSystem
open PlantGrowthSystem
open VisionSystem
open WorldMapRenderer


let renderType : RenderTypes = RenderTypes.World 

let aiOnly : bool = true

let format = LoadAndSave.SaveGameFormats.XML

let wmr = new WorldMapRenderer()

let g = new Game(wmr.UpdateWorld, wmr.UpdateEntity, renderType, format)

let startingEntities = 
    MakeMap g.Entities
    |> Array.append (MakeGrasses g.Entities 5)
    |> Array.append (MakeRabbits g.Entities (not aiOnly) 3)

let systems =
    [|
        EntitySystem("Entity System", true, g.Entities, g.Events).Abstract
        
        ControllerSystem("Controller System", true, g.Entities, g.Events).Abstract
        EatingSystem("Eating System", true, g.Entities, g.Events).Abstract
        FoodSystem("Food System", true, g.Entities, g.Events).Abstract
        KillSystem("Kill System", true, g.Entities, g.Events).Abstract
        MatingSystem("Mating System", true, g.Entities, g.Events).Abstract
        MovementSystem("Movement System", true, g.Entities, g.Events).Abstract
        PlantGrowthSystem("Plant Growth System", true, g.Entities, g.Events).Abstract
        VisionSystem("Vision System", true, g.Entities, g.Events).Abstract // 60s
    |]

g.Start systems startingEntities ""

//g.Start systems Array.empty "Save_201911221508"



