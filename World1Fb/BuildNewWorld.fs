module BuildNewWorld
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open LocationTypes
open EntityManager

let MakeMap (enm:EntityManager) = 
    let AddTerrain l = 
        let eid = enm.NewEntityID()
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
                Form { ID = enm.NewComponentID(); EntityID = eid; Born = RoundNumber(0u); CanSeePast = canSeePast; IsPassable = t.IsPassable; Name = t.ToString(); Symbol = t.Symbol; Location = l }
                Terrain { ID = enm.NewComponentID(); EntityID = eid; Terrain = t }
            |] 
            // I left this mechanic in place because there will be some component that is appropriate to add to Terrain--like a burrow
            //match food.IsSome with
            //| false -> ()
            //| true -> baseTerrain <- Array.append baseTerrain [|food.Value:>AbstractComponent|]
        baseTerrain
    MapLocations |> Array.Parallel.map AddTerrain


let MakeGrasses (enm:EntityManager) n =
    let MakeGrass x y =
        let eid = enm.NewEntityID()
        [| 
            Food { ID = enm.NewComponentID(); EntityID = eid; FoodType = Food_Carrot; Quantity = 20; QuantityMax = 20 }
            Form { ID = enm.NewComponentID(); EntityID = eid; Born = RoundNumber(0u); CanSeePast = true; IsPassable = true; Name = Food_Carrot.ToString(); Symbol = Food_Carrot.Symbol.Value; Location = {X=x;Y=y;Z=0} }
            PlantGrowth { ID = enm.NewComponentID(); EntityID = eid; GrowsInTerrain = [|Dirt|]; RegrowRate = 0.1; ReproductionRate = 0.25; ReproductionRange = 5; ReproductionRequiredFoodQuantity = 0.75 }
        |] 
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (fun i -> MakeGrass (random.Next(0,MapWidth)) (random.Next(0,MapHeight))) 


let MakeRabbits (enm:EntityManager) n = 
    let MakeRabbit x y rnd n = 
        let eid = enm.NewEntityID()
        let controller = 
            match n with
            | 1 -> Controller { ID = enm.NewComponentID(); EntityID = eid; ControllerType = AI_Random; CurrentAction = Idle; CurrentActions = [|Idle|]; PotentialActions = [|Idle|] }
            | _ -> Controller { ID = enm.NewComponentID(); EntityID = eid; ControllerType = AI_Random; CurrentAction = Idle; CurrentActions = [|Idle|]; PotentialActions = [|Idle|] }
        let matingStatus = if n = 1 || rnd = 0 then Male else Female
        let symbol = if matingStatus = Male then 'R' else 'r'
        let location = { X = x; Y = y; Z = 0 }
        let visionRange = 5 //10
        let rangeTemplate = RangeTemplate2D visionRange
        let visionMap = LocationsWithinRange2D location (RangeTemplate2D visionRange)
        let baseBunny = 
            [|
                controller
                Eating { ID = enm.NewComponentID(); EntityID = eid; Calories = 150; CaloriesPerDay = 300; Foods = [|Food_Carrot;Food_Grass|]; Quantity = 75; QuantityMax = 150; QuantityPerAction = 1 }
                Form { ID = enm.NewComponentID(); EntityID = eid; Born = RoundNumber(0u); CanSeePast = true; IsPassable = true; Name = "rabbit"; Symbol = symbol; Location = location }
                Mating { ID = enm.NewComponentID(); EntityID = eid; ChanceOfReproduction = 0.9; LastMatingAttempt = RoundNumber(0u); MatingStatus = matingStatus; Species = Rabbit }
                Movement { ID = enm.NewComponentID(); EntityID = eid; MovesPerTurn = 1 }
                Vision { ID = enm.NewComponentID(); EntityID = eid; Range = visionRange; RangeTemplate = rangeTemplate; ViewedHistory = Map.empty; (*ViewedHistory2 = Map.empty;*) VisibleLocations = Map.empty; LocationsWithinRange = visionMap }
            |]
        baseBunny
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (MakeRabbit (random.Next(0,MapWidth)) (random.Next(0,MapHeight)) (random.Next(0,2))) 
    
