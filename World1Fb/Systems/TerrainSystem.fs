module TerrainSystem
open GameManager
open SystemManager


type TerrainSystem(description:string, game:Game, isActive:bool) =
    inherit AbstractSystem(description,isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    //Disabled

    override me.Update round = 
        ()


        