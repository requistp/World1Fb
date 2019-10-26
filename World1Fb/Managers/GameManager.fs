module GameManager
open AbstractComponent
open AbstractSystem
open CommonGenericFunctions
open EntityManager
open EventManager
open EatingComponent
open FoodComponent
open FrameManager
open EventTypes
open InputHandler
open PlantGrowthComponent
open SystemManager
open System


type Game(renderer:EntityManager->uint32->unit, renderer_SetContent:(string*string)[]->bool->Async<unit>, renderer_SetDisplay:string->unit, renderer_Display:string->unit, wmr:EntityManager->unit, wmrKeys:ConsoleKey->unit) =
    let frameMan = new FrameManager()
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan, (fun () -> frameMan.Round))
    let systemMan = new SystemManager(eventMan)
    let inputMan = new InputHandler(eventMan, entityMan, frameMan, systemMan, renderer_SetDisplay, wmrKeys)
 
    member this.EventManager = eventMan
    member this.EntityManager = entityMan
    member this.FrameManager = frameMan
    member this.SystemManager = systemMan
    member this.Round = (uint32 frameMan.Round) // - 1u)

    member private this.assignController =
        match Component_Controller |> entityMan.Components.List with
        | [||] -> None
        | l -> Some l.[0]

    member private this.setInitialForms (initialForms:AbstractComponent[][]) = 
        initialForms 
        |> Array.iter (fun cts -> if cts.Length > 0 then eventMan.QueueEvent (EventData_CreateEntity(cts.[0].EntityID,cts))) // Can't Parallel

    member private this.gameLoop =
        systemMan.UpdateSystems

        eventMan.ProcessEvents
        
        frameMan.AddFrame entityMan.Entities entityMan.MaxEntityID //Array.empty //geResults

        //entityMan.Entities.List()
        //renderer_SetContent [| ("World Map",entityMan.ToDisplayString); ("Game Events List",frameMan.GERs_ToString GEListType.Last10FramesExcludingFirst) |] true |> Async.Start
        wmr entityMan
        //printfn "Round:%i" this.Round
        
    member this.Start (ss:AbstractSystem[]) (initialForms:AbstractComponent[][]) = 
        systemMan.Initialize ss
        this.setInitialForms initialForms
        
        this.gameLoop

        this.assignController |> inputMan.SetEntityID

        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then this.gameLoop
            r <- inputMan.AwaitKeyboardInput






            (*
            member private this.PrintController =
                match inputMan.EntityID with
                | None -> ()
                | Some eid ->
                    let e = entityMan.GetComponent<EatingComponent> eid
                    printfn "Eater Quantity:%i     " e.Quantity
                    printfn "Calories:%i    " e.Calories

            member private this.PrintPlant =
                let eids = (entityMan.EntitiesWithComponent Component_PlantGrowth)
                match eids with 
                | [||] -> printfn "Plant Quantity: no plants"
                | _ ->
                    let e = entityMan.GetComponent<FoodComponent> eids.[0]
                    printfn "Plant Quantity:%i     " e.Quantity
*)