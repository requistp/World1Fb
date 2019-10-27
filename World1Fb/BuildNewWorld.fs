module BuildNewWorld
open AbstractComponent
open CalendarTimings
open CommonGenericFunctions
open ControllerComponent
open EatingComponent
open EntityManager
open FoodComponent
open FormComponent
open LocationTypes
open MovementComponent
open PlantGrowthComponent
open TerrainComponent


let MakeGrasses (enm:EntityManager2) n =
    let MakeGrass x y =
        let eid = enm.EntityID_New
        [| 
            FoodComponent(eid, Food_Carrot, 20).Abstract
            FormComponent(eid, true, Food_Carrot.ToString(), Food_Grass.Symbol.Value, {X=x;Y=y;Z=0}).Abstract
            PlantGrowthComponent(eid, [|Dirt|], 0.1, 0.25, 5, 0.75).Abstract
        |] 
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeGrass (random.Next(0,MapWidth)) (random.Next(0,MapHeight))) //Can't Parallel
    |> Array.rev

let MakeMap (enm:EntityManager2) = 
    let AddTerrain x y = 
        let eid = enm.EntityID_New
        let t = match random.Next(1,50) with
                | 1 -> Rock
                | _ -> Dirt
        let mutable baseTerrain =
            [| 
                FormComponent(eid, t.IsPassable, t.ToString(), t.Symbol, {X=x;Y=y;Z=0}).Abstract
                TerrainComponent(eid, t).Abstract 
            |] 
        // I left this mechanic in place because there will be some component that is appropriate to add to Terrain--like a burrow
        //match food.IsSome with
        //| false -> ()
        //| true -> baseTerrain <- Array.append baseTerrain [|food.Value:>AbstractComponent|]
        baseTerrain

    let mutable tmap = Array.empty<AbstractComponent[]>
    for y in [0..MapHeight-1] do
        for x in [0..MapWidth-1] do
            tmap <- tmap |> Array.append [| AddTerrain x y |]
    tmap


let MakeRabbits (enm:EntityManager2) n = 
    let MakeRabbit x y n = 
        let eid = enm.EntityID_New
        let cont = [| ControllerComponent(eid).Abstract  |]
        let baseBunny = 
            [|
                EatingComponent(eid, [|Food_Carrot;Food_Grass|], 150, 1, 300).Abstract
                FormComponent(eid, true, "rabbit", 'r', {X=x;Y=y;Z=0}).Abstract
                MovementComponent(eid, 1).Abstract
                //sight
                //health        
            |]
        match n = 1 with
        | false -> baseBunny
        | true -> baseBunny |> Array.append cont
    match n with 
    | 0 -> Array.empty<AbstractComponent[]>
    | _ -> [1..n] |> List.toArray |> Array.map (fun i -> MakeRabbit (random.Next(0,MapWidth)) (random.Next(0,MapHeight)) i) //Can't Parallel
    |> Array.rev

