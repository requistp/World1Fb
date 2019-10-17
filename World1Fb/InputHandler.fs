module InputHandler
open AbstractComponent
open EatingComponent
open EntityManager
open EventManager
open FoodComponent
open FormComponent
open GameEvents
open LocationTypes
open MovementComponent
open System
open SystemManager


type InputHandler(evm:EventManager, enm:EntityManager, sysm:SystemManager) =
    let mutable _entityID = None

    member private this.HasRequiredComponents (cts:ComponentTypes[]) =
        match _entityID with
        | None -> None
        | Some eid -> match enm.EntityHasAllComponents cts eid with
                      | false -> None
                      | true -> Some eid

    member private this.keyPressed_Eat =
        // This feels a bit kludgy as if there are two edible foods at the location there should be a UI for selecting which one to eat
        let foodAtLocation (eid:uint32) = 
            let eaterForm = enm.GetComponent<FormComponent> eid
            let eaterEating = enm.GetComponent<EatingComponent> eid
            let foodsAtLocation = 
                enm.EntitiesAtLocation eaterForm.Location // Food here
                |> Array.filter (fun eid -> eid <> eaterForm.EntityID) // Not me
                |> enm.TryGetComponentForEntities<FoodComponent>
                |> Array.filter (fun f -> eaterEating.Foods |> Array.exists (fun ft -> ft = f.FoodType)) //Types I can eat
                |> Array.filter (fun f -> f.Quantity > 0) // Food remaining
                |> Array.sortByDescending (fun x -> x.FoodType.Calories) // Highest caloric food first
            match foodsAtLocation with
            | [||] -> None
            | a -> Some a.[0]
        match this.HasRequiredComponents [|ComponentTypes.Comp_Eating|] with
        | None -> ()
        | Some eid -> match foodAtLocation eid with
                      | None -> ()
                      | Some f -> evm.QueueEvent(Event_Action_Eat(eid,f.EntityID))
                                           
    member private this.keyPressed_Movement d = 
        match this.HasRequiredComponents [| ComponentTypes.Comp_Form; ComponentTypes.Comp_Movement |] with
        | None -> ()
        | Some eid -> evm.QueueEvent (Event_Action_Movement(eid,d))

    member private this.onKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> this.keyPressed_Movement North 
        | ConsoleKey.DownArrow -> this.keyPressed_Movement South
        | ConsoleKey.LeftArrow -> this.keyPressed_Movement West 
        | ConsoleKey.RightArrow -> this.keyPressed_Movement East
        | ConsoleKey.E -> this.keyPressed_Eat
        | _ -> ()  

        while Console.KeyAvailable do //Might helpclear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
            
    member this.SetEntityID eid = _entityID <- eid

    member this.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 1

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> this.onKeyPressed k
               true
