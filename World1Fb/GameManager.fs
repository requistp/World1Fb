module GameManager
open AbstractComponent
open AbstractSystem
open ControllerComponent
open EntityManager
open EventManager
open EatingComponent
open FoodComponent
open FrameManager
open GameEvents
open InputHandler
open PlantGrowthComponent
open SystemManager


type Game(renderer:EntityManager->int->uint32->unit) =
    let frameMan = new FrameManager()
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager(eventMan)
    let inputMan = new InputHandler(eventMan, entityMan, frameMan, systemMan)
 
    member this.EventManager = eventMan
    member this.EntityManager = entityMan
    member this.SystemManager = systemMan
    member this.Round = frameMan.Count - 1

    member private this.assignController =
        match ControllerComponent.Type |> entityMan.EntitiesWithComponent with
        | [||] -> None
        | l -> Some l.[0]

    member private this.PrintController =
        match inputMan.EntityID with
        | None -> ()
        | Some eid ->
            let e = entityMan.GetComponent<EatingComponent> eid
            printfn "Eater Quantity:%i     " e.Quantity
            printfn "Calories:%i    " e.Calories

    member private this.PrintPlant =
        let eid = (entityMan.EntitiesWithComponent PlantGrowthComponent.Type).[0]
        let e = entityMan.GetComponent<FoodComponent> eid
        printfn "Plant Quantity:%i     " e.Quantity

    member private this.setInitialForms (initialForms:AbstractComponent[][]) = 
        initialForms 
        |> Array.iter (fun cts -> eventMan.QueueEvent (Event_CreateEntity(cts))) // Can't Parallel

    member private this.gameLoop =
        systemMan.UpdateSystems (this.Round)
        let geResults = eventMan.ProcessEvents
        let setResult = entityMan.SetToNext
        let f = frameMan.AddFrame entityMan.Entities entityMan.MaxEntityID geResults setResult
        renderer entityMan frameMan.Count f.Number
        this.PrintController
        this.PrintPlant

    member this.Start (ss:AbstractSystem[]) (initialForms:AbstractComponent[][]) = 
        entityMan.Initialize
        systemMan.Initialize ss
        this.setInitialForms initialForms
        
        this.gameLoop

        this.assignController |> inputMan.SetEntityID

        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then this.gameLoop
            r <- inputMan.AwaitKeyboardInput

