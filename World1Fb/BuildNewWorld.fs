module BuildNewWorld
open AbstractComponent
open CommonGenericFunctions
open ControllerComponent
open EntityManager
open FormComponent
open LocationTypes
open MovementComponent
open TerrainComponent


let MakeMap (enm:EntityManager) = 
    let AddTerrain x y = 
        let eid = enm.NewEntityID
        let t = match random.Next(1,20) with
                | 1 -> Rock
                | _ -> Dirt

        [| 
            [| 
                FormComponent(eid, t.IsPassable, t.ToString(), t.Symbol, {X=x;Y=y;Z=0}) :> AbstractComponent
                TerrainComponent(eid, t) :> AbstractComponent 
            |] 
        |]
    
    let mutable tmap = Array.empty<AbstractComponent[]>

    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            tmap <- tmap |> Array.append (AddTerrain x y)

    tmap


let MakeRabbit (enm:EntityManager) x y = 
    let eid = enm.NewEntityID
    [|
        ControllerComponent(eid) :> AbstractComponent
        FormComponent(eid, true, "rabbit", 'r', {X=x;Y=y;Z=0}) :> AbstractComponent
        MovementComponent(eid, 1) :> AbstractComponent
        //sight
        //health        
    |] 


let MakeRabbits (enm:EntityManager) n = 
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeRabbit enm (random.Next(0,MapWidth-1)) (random.Next(0,MapHeight-1)))

