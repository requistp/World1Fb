module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityManager
open EventManager
open EatingComponent
open FoodComponent
open GameEvents
open InputHandler
open SystemManager

type Frame = 
    {
        Number : uint32
        Entities : Map<uint32,AbstractComponent[]>
        MaxEntityID : uint32 
        SCL : SystemChangeLog
        GameEvents : AbstractGameEvent[]
    } with 
    static member empty = 
        { 
            Number = 0u
            Entities = Map.empty
            MaxEntityID = 0u
            SCL = SystemChangeLog.empty
            GameEvents = Array.empty
        }


type FrameManager() =
    let mutable _frames = [| Frame.empty |]
    member this.AddFrame (entities:Map<uint32,AbstractComponent[]>) (maxEntityID:uint32) (ges:AbstractGameEvent[]) (scl:SystemChangeLog) =
        let f = 
            { 
                Number = (uint32 _frames.Length)
                Entities = entities
                MaxEntityID = maxEntityID
                SCL = scl
                GameEvents = ges
            } 
        _frames <- Array.append _frames [|f|]
        f
    member this.Count = _frames.Length


type Game(renderer:EntityManager->int->uint32->unit) =
    let frameMan = new FrameManager()
    let eventMan = new EventManager()
    let entityMan = new EntityManager(eventMan)
    let systemMan = new SystemManager(eventMan)
    let inputMan = new InputHandler(eventMan,entityMan)
 
    member this.EventManager = eventMan
    member this.EntityManager = entityMan
    member this.SystemManager = systemMan

    member private this.assignController =
        match Controller |> entityMan.EntitiesWithComponent with
        | [||] -> None
        | l -> Some l.[0]

    member private this.PrintGrass =
        let f = 
            (entityMan.EntitiesWithComponent Food).[0]
            |> (entityMan.GetComponent Food) :?> FoodComponent

        let e = 
            (entityMan.EntitiesWithComponent Eating).[0]
            |> (entityMan.GetComponent Eating) :?> EatingComponent

        printfn "Quanity:%i     " f.Quantity
        printfn "Calories:%i    " e.Calories_Current

    member private this.gameLoop =
        let ges = eventMan.ProcessEvents
        let scl = systemMan.UpdateSystems
        //I don't know if I need this, not yet... let gel = _eventManager.ProcessEvents |> Array.append gel0
        let finalSCL = entityMan.ProcessSystemChangeLog scl
        let f = frameMan.AddFrame entityMan.Entities entityMan.MaxEntityID ges finalSCL
        renderer entityMan frameMan.Count f.Number
        this.PrintGrass

    member this.Start (ss:AbstractSystem[]) = 
        entityMan.Initialize
        systemMan.Initialize ss

        this.gameLoop

        this.assignController |> inputMan.SetEntityID

        while inputMan.AwaitKeyboardInput do
            this.gameLoop
            
