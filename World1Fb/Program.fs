open BuildNewWorld
open CommonGenericFunctions
open EntityComponentManager
open FormComponent
open FormSystem
open GameManager
open LocationTypes
open MovementSystem
open Renderer
open SystemManager
open TerrainComponent
open TerrainSystem

let g = new Game(RenderFrame)

let systems = 
    [|
        FormSystem(g, true, MakeRabbits 1) :> AbstractSystem
        MovementSystem(g, true) :> AbstractSystem
        TerrainSystem(g, true, MakeMap) :> AbstractSystem
    |]

g.SystemManager.RegisterSystems systems

g.Start 

