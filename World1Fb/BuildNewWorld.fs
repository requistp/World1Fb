module BuildNewWorld
open CommonGenericFunctions
open LocationTypes
open Renderer
open GameManager
open FormSystem
open TerrainSystem
open Components
open TerrainComponent
open FormComponent
open EntityComponentManager

let MakeMap ecd = 
    let mutable newecd = ecd
    
    let AddTerrain x y = Entity.Create newecd [ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })]

    for x in [0us..MapWidth - 1us] do
        for y in [0us..MapHeight - 1us] do
            newecd <- AddTerrain x y
    newecd

let MakeRabbit ecd x y = 
    //let rx = (uint16 (random.Next(0,MapWidthInt-1)))
    //let ry = (uint16 (random.Next(0,MapHeightInt-1)))
    let form = ComponentType.Form { IsPassable=true; Name="rabbit"; Symbol='r'; Location={X=x;Y=y} }
    let move = ComponentType.Movement { MovesPerTurn = 2uy }
    //sight
    //health
    let ctl = [
        form
        move
    ]
    Entity.Create ecd ctl

let MakeRabbits ecd n = 
    let mutable newecd = ecd
    match n with 
    | 0 -> ecd
    | _ -> for x in [1..n] do
               newecd <- MakeRabbit newecd (uint16 (random.Next(0,MapWidthInt-1))) (uint16 (random.Next(0,MapHeightInt-1)))
           newecd
