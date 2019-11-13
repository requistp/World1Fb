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
        let t = 
            match random.Next(1,50) with
            | 1 -> Rock
            | _ -> Dirt
        let canSeePast = 
            match t with
            | Rock -> false
            | _ -> true
            
        let mutable baseTerrain =
            [| 
                Form { EntityID = eid; Born = 0u; CanSeePast = canSeePast; IsPassable = t.IsPassable; Name = t.ToString(); Symbol = t.Symbol; Location = l }
                Terrain { EntityID=eid; Terrain=t }
            |] 
            // I left this mechanic in place because there will be some component that is appropriate to add to Terrain--like a burrow
            //match food.IsSome with
            //| false -> ()
            //| true -> baseTerrain <- Array.append baseTerrain [|food.Value:>AbstractComponent|]
        baseTerrain
    MapLocations |> Array.Parallel.map (fun l -> AddTerrain l)


let MakeGrasses (enm:EntityManager) n =
    let MakeGrass x y =
        let eid = enm.GetNewID
        [| 
            Food { EntityID = eid; FoodType = Food_Carrot; Quantity = 20; QuantityMax = 20 }
            Form { EntityID = eid; Born = 0u; CanSeePast = true; IsPassable = true; Name = Food_Carrot.ToString(); Symbol = Food_Carrot.Symbol.Value; Location = {X=x;Y=y;Z=0} }
            PlantGrowth { EntityID = eid; GrowsInTerrain = [|Dirt|]; RegrowRate = 0.1; ReproductionRate = 0.25; ReproductionRange = 5; ReproductionRequiredFoodQuantity = 0.75 }
        |] 
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (fun i -> MakeGrass (random.Next(0,MapWidth)) (random.Next(0,MapHeight))) 


let MakeRabbits (enm:EntityManager) n = 
    let MakeRabbit x y n rnd = 
        let eid = enm.GetNewID
        let cont = [| Controller { EntityID = eid; Actions = [||]; CurrentActions = [||] } |]
        let matingStatus = if n = 1 || rnd = 0 then Male else Female
        let symbol = if matingStatus = Male then 'R' else 'r'
        let location = { X = x; Y = y; Z = 0 }
        let visionRange = 5
        let rangeTemplate = RangeTemplate2D visionRange
        let visionMap = LocationsWithinRange2D location (RangeTemplate2D visionRange)
        let viewedMap = visionMap |> Array.fold (fun (v:Map<LocationDataInt,uint32>) l -> v.Add(l,0u)) Map.empty
        let baseBunny = 
            [|
                Eating { EntityID = eid; Calories = 150; CaloriesPerDay = 300; Foods = [|Food_Carrot;Food_Grass|]; Quantity = 75; QuantityMax = 150; QuantityPerAction = 1 }
                Form { EntityID = eid; Born = 0u; CanSeePast = true; IsPassable = true; Name = "rabbit"; Symbol = symbol; Location = location }
                Mating { EntityID = eid; ChanceOfReproduction = 0.9; LastMatingAttempt = 0u; MatingStatus = matingStatus; Species = Rabbit }
                Memory { EntityID = eid; Memories = Map.empty; Retention = 30u }
                Movement { EntityID = eid; MovesPerTurn = 1 }
                Vision { EntityID = eid; Range = visionRange; RangeTemplate = rangeTemplate; ViewedMap = viewedMap; ViewableMap = visionMap; VisionMap = visionMap }
            |]
        match n = 1 with
        | false -> baseBunny
        | true -> baseBunny |> Array.append cont
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (fun i -> MakeRabbit (random.Next(0,MapWidth)) (random.Next(0,MapHeight)) i (random.Next(0,2))) 


