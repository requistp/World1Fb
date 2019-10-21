module TerrainSystem
open AbstractSystem
open CommonGenericFunctions
open EntityManager
open EventManager
open EventTypes
open GameManager
open LocationTypes
open TerrainComponent

type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

