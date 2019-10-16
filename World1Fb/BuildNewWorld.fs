module BuildNewWorld
open AbstractComponent
open CommonGenericFunctions
open ControllerComponent
open EatingComponent
open EntityManager
open FoodComponent
open FormComponent
open LocationTypes
open MovementComponent
open TerrainComponent


let MakeGrasses (enm:EntityManager) n =
    let MakeGrass x y =
        let eid = enm.NewEntityID
        [| 
            FoodComponent(eid, Grass, 20) :> AbstractComponent
            FormComponent(eid, true, "grass", Grass.Symbol.Value, {X=x;Y=y;Z=0}) :> AbstractComponent
        |] 
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeGrass (random.Next(0,MapWidth-1)) (random.Next(0,MapHeight-1))) //Can't Parallel


let MakeMap (enm:EntityManager) = 
    let AddTerrain x y = 
        let eid = enm.NewEntityID
        let t = match random.Next(1,50) with
                | 1 -> Rock
                | _ -> Dirt
        let mutable baseTerrain =
            [| 
                FormComponent(eid, t.IsPassable, t.ToString(), t.Symbol, {X=x;Y=y;Z=0}) :> AbstractComponent
                TerrainComponent(eid, t) :> AbstractComponent 
            |] 
        // I left this mechanic in place because there will be some component that is appropriate to add to Terrain--like a burrow
        //match food.IsSome with
        //| false -> ()
        //| true -> baseTerrain <- Array.append baseTerrain [|food.Value:>AbstractComponent|]
        baseTerrain

    let mutable tmap = Array.empty<AbstractComponent[]>
    for x in [0..MapWidth-1] do
        for y in [0..MapHeight-1] do
            tmap <- tmap |> Array.append [| AddTerrain x y |]
    tmap


let MakeRabbits (enm:EntityManager) n = 
    let MakeRabbit x y = 
        let eid = enm.NewEntityID
        [|
            ControllerComponent(eid) :> AbstractComponent
            EatingComponent(eid, 1, [|Carrot;Grass|], 10, 5) :> AbstractComponent
            FormComponent(eid, true, "rabbit", 'r', {X=x;Y=y;Z=0}) :> AbstractComponent
            MovementComponent(eid, 1) :> AbstractComponent
            //sight
            //health        
        |]    
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeRabbit (random.Next(0,MapWidth-1)) (random.Next(0,MapHeight-1))) //Can't Parallel

