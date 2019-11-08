module TerrainSystem
open GameManager
open SystemManager


type TerrainSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    //Disabled

    override _.ToString = "TerrainSystem"

    override me.Update round = 
        ()


        