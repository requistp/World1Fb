module BuildNewWorld
open AbstractComponent
open CommonGenericFunctions
open ControllerComponent
open FormComponent
open LocationTypes
open MovementComponent
open TerrainComponent
open EntityComponentManager

let MakeMap ecd = 
    let mutable newecd = ecd
    
    let AddTerrain x y = Entity.Create newecd [ TerrainComponent(TerrainType.Dirt, {X=x; Y=y}) ]

    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            newecd <- AddTerrain x y
    newecd

let MakeRabbit ecd x y = 
    [
        ControllerComponent() :> AbstractComponent
        FormComponent(true, "rabbit", 'r', {X=x;Y=y}) :> AbstractComponent
        MovementComponent(1) :> AbstractComponent
        //sight
        //health        
    ] |> Entity.Create ecd

let MakeRabbits ecd n = 
    match n with 
    | 0 -> ecd
    | _ -> let mutable newecd = ecd
           for x in [1..n] do
               newecd <- MakeRabbit newecd (random.Next(0,MapWidth-1)) (random.Next(0,MapHeight-1))
           newecd
