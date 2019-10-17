module GameManager
open AbstractComponent
open AbstractSystem
open ControllerComponent
open EntityManager
open EventManager
open EatingComponent
open FoodComponent
open FormComponent
open GameEvents
open InputHandler
open SystemManager

type Frame = 
    {
        Number : uint32
        Entities : Map<uint32,AbstractComponent[]>
        MaxEntityID : uint32 
        GEResults : (AbstractGameEvent * Result<string option,string>)[]
        SetResult : Result<string option,string>
    } with 
    static member empty = 
        { 
            Number = 0u
            Entities = Map.empty
            MaxEntityID = 0u
            GEResults = Array.empty
            SetResult = Ok None
        }


type FrameManager() =
    let mutable _frames = [| Frame.empty |]
    member this.AddFrame (entities:Map<uint32,AbstractComponent[]>) (maxEntityID:uint32) (geResults:(AbstractGameEvent * Result<string option,string>)[]) (setResult:Result<string option,string>) =
        let f = 
            { 
                Number = (uint32 _frames.Length)
                Entities = entities
                MaxEntityID = maxEntityID
                GEResults = geResults
                SetResult = setResult
            } 
        _frames <- Array.append _frames [|f|]
        f
    member this.Count = _frames.Length


type Game(renderer:EntityManager->int->uint32->unit) =
    let frameMan = new FrameManager()
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager(eventMan)
    let inputMan = new InputHandler(eventMan, entityMan, systemMan)
 
    member this.EventManager = eventMan
    member this.EntityManager = entityMan
    member this.SystemManager = systemMan

    member private this.assignController =
        match ControllerComponent.Type |> entityMan.EntitiesWithComponent with
        | [||] -> None
        | l -> Some l.[0]

    member private this.PrintGrass =
        let f = (entityMan.EntitiesWithComponent FoodComponent.Type).[0] |> entityMan.GetComponent<FoodComponent>
        let e = (entityMan.EntitiesWithComponent EatingComponent.Type).[0] |> entityMan.GetComponent<EatingComponent>
        printfn "Quanity:%i     " f.Quantity
        printfn "Calories:%i    " e.Calories

    member private this.setInitialForms (initialForms:AbstractComponent[][]) = 
        initialForms 
        |> Array.iter (fun cts -> eventMan.QueueEvent (Event_CreateEntity(cts))) // Can't Parallel

    member private this.gameLoop =
        systemMan.UpdateSystems
        let geResults = eventMan.ProcessEvents
        let setResult = entityMan.SetToNext
        let f = frameMan.AddFrame entityMan.Entities entityMan.MaxEntityID geResults setResult
        renderer entityMan frameMan.Count f.Number
        this.PrintGrass

    member this.Start (ss:AbstractSystem[]) (initialForms:AbstractComponent[][]) = 
        entityMan.Initialize
        systemMan.Initialize ss
        this.setInitialForms initialForms

        //let f0 = entityMan.TestGet<FormComponent> 0u 

        this.gameLoop

        this.assignController |> inputMan.SetEntityID

        while inputMan.AwaitKeyboardInput do
            this.gameLoop
            

