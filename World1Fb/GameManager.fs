module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityManager
open EventManager
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
    static member New(fnum:uint32, entities:Map<uint32,AbstractComponent[]>, maxEntityID:uint32, scl:SystemChangeLog, ges:AbstractGameEvent[]) = 
        { 
            Number = fnum
            Entities = entities
            MaxEntityID = maxEntityID
            SCL = scl
            GameEvents = ges
        }

type Game(renderer:EntityManager->int->unit) =
    let mutable _frames = [| Frame.empty |]
    let _eventManager = new EventManager()
    let _entityManager = new EntityManager(_eventManager)
    let _systemManager = new SystemManager(_eventManager)
    let _inputHandler = new InputHandler(_eventManager)
 
    member this.EventManager = _eventManager
    member this.EntityManager = _entityManager
    member this.SystemManager = _systemManager

    member private this.assignController =
        match Controller |> _entityManager.GetAllWithComponent with
        | [||] -> None
        | l -> Some l.[0]
    member private this.gameLoop =
        let ges = _eventManager.ProcessEvents
        let scl = _systemManager.UpdateSystems
        let e,meid = _entityManager.ProcessSystemChangeLog scl
        //I don't know if I need this, not yet... let gel = _eventManager.ProcessEvents |> Array.append gel0
        _frames <- [|Frame.New((uint32 _frames.Length), e, meid, scl, ges)|] |> Array.append _frames
        renderer _entityManager _frames.Length
    
    member this.Start (ss:AbstractSystem[]) = 
        _entityManager.Initialize
        _systemManager.Initialize ss

        this.gameLoop
        this.assignController |> _inputHandler.SetEntityID

        while _inputHandler.AwaitKeyboardInput do
            this.gameLoop
            
