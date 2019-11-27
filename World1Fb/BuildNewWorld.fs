module BuildNewWorld
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EatingComponent
open EntityManager
open FoodComponent
open FormComponent
open LocationTypes
open MatingComponent
open MovementComponent
open PlantGrowthComponent
open TerrainComponent
open VisionComponent

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
                Form(FormComponent(enm.NewComponentID(), eid, RoundNumber(0u), canSeePast, t.IsPassable, l, t.ToString(), t.Symbol))
                //Form { ID = enm.NewComponentID(); EntityID = eid; Born = RoundNumber(0u); CanSeePast = canSeePast; IsPassable = t.IsPassable; Name = t.ToString(); Symbol = t.Symbol; Location = l }
                //Terrain { ID = enm.NewComponentID(); EntityID = eid; Terrain = t }
                Terrain(TerrainComponent(enm.NewComponentID(), eid, t))
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
            Food(FoodComponent(enm.NewComponentID(), eid, Food_Carrot, 20, 20))
            //Food { ID = enm.NewComponentID(); EntityID = eid; FoodType = Food_Carrot; Quantity = 20; QuantityMax = 20 }
            Form(FormComponent(enm.NewComponentID(), eid, RoundNumber(0u), true, true, LocationDataInt(x,y,0), Food_Carrot.ToString(), Food_Carrot.Symbol.Value))
            //PlantGrowth { ID = enm.NewComponentID(); EntityID = eid; GrowsInTerrain = [|Dirt|]; RegrowRate = 0.1; ReproductionRate = 0.25; ReproductionRange = 5; ReproductionRequiredFoodQuantity = 0.75 }
            PlantGrowth(PlantGrowthComponent(enm.NewComponentID(), eid, [|Dirt|], 0.1, 0.25, 5, 0.75))
        |] 
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (fun i -> MakeGrass (random.Next(0,MapWidth)) (random.Next(0,MapHeight))) 


let MakeRabbits (enm:EntityManager) (firstIsHuman:bool) n = 
    let MakeRabbit x y rnd n = 
        let eid = enm.NewEntityID()
        let controller = 
            match n with
            | 1 -> Controller(ControllerComponent(enm.NewComponentID(), eid, (if firstIsHuman then Keyboard else AI_Random), Idle, [|Idle|], [|Idle|]))
                //Controller { ID = enm.NewComponentID(); EntityID = eid; ControllerType = (if firstIsHuman then Keyboard else AI_Random); CurrentAction = Idle; CurrentActions = [|Idle|]; PotentialActions = [|Idle|] }
            | _ -> Controller(ControllerComponent(enm.NewComponentID(), eid, AI_Random, Idle, [|Idle|], [|Idle|]))
                //Controller { ID = enm.NewComponentID(); EntityID = eid; ControllerType = AI_Random; CurrentAction = Idle; CurrentActions = [|Idle|]; PotentialActions = [|Idle|] }
        let matingStatus = if n = 1 || rnd = 0 then Male else Female
        let symbol = if matingStatus = Male then 'R' else 'r' // Handy for debugging: n.ToString().ToCharArray().[0]
        let location = LocationDataInt(x,y,0)
        let visionRange = 10
        let rangeTemplate = RangeTemplate2D visionRange
        let visionMap = LocationsWithinRange2D location (RangeTemplate2D visionRange)
        let visionCalculationType = Shadowcast1
        let baseBunny = 
            [|
                controller
                Eating(EatingComponent(enm.NewComponentID(), eid, 150, 300, [|Food_Carrot;Food_Grass|], 75, 150, 1))
                //Eating { ID = enm.NewComponentID(); EntityID = eid; Calories = 150; CaloriesPerDay = 300; Foods = [|Food_Carrot;Food_Grass|]; Quantity = 75; QuantityMax = 150; QuantityPerAction = 1 }
                Form(FormComponent(enm.NewComponentID(), eid, RoundNumber(0u), true, true, location, "rabbit", symbol))
                //Form { ID = enm.NewComponentID(); EntityID = eid; Born = RoundNumber(0u); CanSeePast = true; IsPassable = true; Name = "rabbit"; Symbol = symbol; Location = location }
                Mating(MatingComponent(enm.NewComponentID(), eid, 0.9, RoundNumber(0u), matingStatus, Rabbit))
                //Mating { ID = enm.NewComponentID(); EntityID = eid; ChanceOfReproduction = 0.9; LastMatingAttempt = RoundNumber(0u); MatingStatus = matingStatus; Species = Rabbit }
                //Movement { ID = enm.NewComponentID(); EntityID = eid; MovesPerTurn = 1 }
                Movement(MovementComponent(enm.NewComponentID(), eid, 1))
                //Vision { ID = enm.NewComponentID(); EntityID = eid; LocationsWithinRange = visionMap; Range = visionRange; RangeTemplate = rangeTemplate; ViewedHistory = Map.empty; VisibleLocations = Map.empty; VisionCalculationType = visionCalculationType }
                Vision(VisionComponent(enm.NewComponentID(), eid, visionMap, visionRange, rangeTemplate, visionCalculationType, Map.empty, Map.empty))
            |]
        baseBunny
    match n with 
    | 0 -> Array.empty<Component[]>
    | _ -> [|1..n|] |> Array.Parallel.map (MakeRabbit (random.Next(0,MapWidth)) (random.Next(0,MapHeight)) (random.Next(0,2))) 
    
