module TerrainSystem
open AbstractComponent
open CommonGenericFunctions
open EntityComponentManager
open EventManager
open GameManager
open LocationTypes
open SystemManager
open TerrainComponent

type TerrainSystem(game:Game, isActive:bool, initialTerrain:AbstractComponent[][]) =
    inherit AbstractSystem(isActive) 

//    static member TerrainAtLocation 
    member private this.setInitialTerrain = 
        initialTerrain 
        |> Array.iter (fun ne -> this.ChangeLog_NewEntity ne)

    override this.Initialize = 
        this.setInitialTerrain
        base.SetToInitialized
        
    override this.Update = 
        base.ChangeLog_PackageAndClose
