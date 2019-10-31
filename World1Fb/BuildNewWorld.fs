module BuildNewWorld
open Component
open ComponentEnums
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open LocationTypes


let MakeMap (enm:EntityManager) = 
    let AddTerrain l = 
        let eid = enm.GetNewID
        let t = //match random.Next(1,50) with
                //| 1 -> Rock
                //| _ -> Dirt
                Dirt
        let mutable baseTerrain =
            [| 
                Form { EntityID=eid; IsPassable=t.IsPassable; Name=t.ToString(); Symbol=t.Symbol; Location=l }
                Terrain { EntityID=eid; Terrain=t }
            |] 
        // I left this mechanic in place because there will be some component that is appropriate to add to Terrain--like a burrow
        //match food.IsSome with
        //| false -> ()
        //| true -> baseTerrain <- Array.append baseTerrain [|food.Value:>AbstractComponent|]
        baseTerrain

    let m2 = 
        MapLocations
        |> Array.Parallel.map (fun l -> AddTerrain l)
    m2


let MakeGrasses (enm:EntityManager) n =
    let MakeGrass x y =
        let eid = enm.GetNewID
        [| 
            Food { EntityID=eid; FoodType=Food_Carrot; Quantity=20; QuantityMax=20 }
            Form { EntityID=eid; IsPassable=true; Name=Food_Carrot.ToString(); Symbol=Food_Carrot.Symbol.Value; Location={X=x;Y=y;Z=0} }
            PlantGrowth { EntityID=eid; GrowsInTerrain=[|Dirt|]; RegrowRate=0.1; ReproductionRate=0.95; ReproductionRange=5; ReproductionRequiredFoodQuantity=0.75 }
        |] 
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [1..n] |> List.toArray |> Array.Parallel.map (fun i -> MakeGrass (random.Next(0,MapWidth)) (random.Next(0,MapHeight))) 
    |> Array.rev


let MakeRabbits (enm:EntityManager) n = 
    let MakeRabbit x y n = 
        let eid = enm.GetNewID
        let cont = [| Controller { EntityID = eid }  |]
        let baseBunny = 
            [|
                Eating { EntityID=eid; Calories=150; CaloriesPerDay=300; Foods=[|Food_Carrot;Food_Grass|]; Quantity=75; QuantityMax=150; QuantityPerAction=1 }
                Form { EntityID=eid; IsPassable=true; Name="rabbit"; Symbol='r'; Location={X=x;Y=y;Z=0} }
                Movement { EntityID=eid; MovesPerTurn=1 }
                //sight
                //health        
            |]
        match n = 1 with
        | false -> baseBunny
        | true -> baseBunny |> Array.append cont
    match n with 
    | 0 -> Array.empty<Component[]>
    //| _ -> [1..n] |> List.toArray |> Array.Parallel.map (fun i -> MakeRabbit (random.Next(0,MapWidth)) (random.Next(0,MapHeight)) i) 
    | _ -> [1..n] |> List.toArray |> Array.Parallel.map (fun i -> MakeRabbit (MapWidth/2) (MapHeight/2) 1) 
    |> Array.rev


