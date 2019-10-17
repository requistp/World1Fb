module TerrainSystem
open AbstractSystem
open CommonGenericFunctions
open EntityManager
open EventManager
open GameEvents
open GameManager
open LocationTypes
open SystemManager
open TerrainComponent

type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Terrain, isActive) 

