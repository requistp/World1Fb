open CommonGenericFunctions
open LocationTypes
open Renderer
open GameManager
open FormSystem
open MovementSystem
open TerrainSystem
open TerrainComponent
open FormComponent
open BuildNewWorld
open EntityComponentManager

let g = new Game((MakeRabbits (MakeMap Entity.EmptyECD) 1), RenderFrame)

let systems = 
    [|
        FormSystem(g,true) :> AbstractSystem
        MovementSystem(g,true) :> AbstractSystem
        TerrainSystem(g,true) :> AbstractSystem
    |]

g.RegisterSystems systems

g.Start 


