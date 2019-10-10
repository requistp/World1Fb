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

let g = new Game((MakeRabbits (MakeMap Entity.EmptyECD) 1), RenderFrame)

let systems = 
    [|
        FormSystem(g,true) :> AbstractSystem
        MovementSystem(g,true) :> AbstractSystem
        TerrainSystem(g,true) :> AbstractSystem
    |]

g.SystemManager.RegisterSystems systems

g.Start 


