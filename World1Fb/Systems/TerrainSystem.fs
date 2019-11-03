module TerrainSystem
open AbstractSystem
open CommonGenericFunctions
open EntityManager
open EventManager
open EventTypes
open GameManager
open LocationTypes

type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    override me.Update = 
        ()


        