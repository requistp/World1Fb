module BuildNewWorld
open AbstractComponent
open CommonGenericFunctions
open ControllerComponent
open FormComponent
open LocationTypes
open MovementComponent
open TerrainComponent


let MakeMap = 
    let AddTerrain x y = 
        let t = match random.Next(1,10) with
                | 1 -> Rock
                | _ -> Dirt

        [| [| TerrainComponent(t, {X=x; Y=y}) :> AbstractComponent |] |]
    
    let mutable tmap = Array.empty<AbstractComponent[]>

    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            tmap <- tmap |> Array.append (AddTerrain x y)

    tmap


let MakeRabbit x y = 
    [|
        ControllerComponent() :> AbstractComponent
        FormComponent(true, "rabbit", 'r', {X=x;Y=y}) :> AbstractComponent
        MovementComponent(1) :> AbstractComponent
        //sight
        //health        
    |] 


let MakeRabbits n = 
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeRabbit (random.Next(0,MapWidth-1)) (random.Next(0,MapHeight-1)))

