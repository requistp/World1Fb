module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityComponentManager
open EventManager
open GameEvents
open InputHandler
open SystemManager

type Frame = 
    {
        Number : uint32
        Entities : Map<uint32,AbstractComponent[]>
        SCL : SystemChangeLog
        GameEvents : AbstractGameEvent[]
    } with 
    static member Empty = 
        { 
            Number = 0u
            Entities = Map.empty
            SCL = SystemChangeLog.empty
            GameEvents = Array.empty
        }

type Game(renderer:EntityManager->int->unit) =
    let mutable _frames = Array.empty:Frame[]
    let _eventManager = new EventManager()
    let _entityManager = new EntityManager(_eventManager)
    let _systemManager = new SystemManager(_eventManager,_entityManager)
    let _input = new InputHandler(_eventManager)

    do
        _frames <- [| Frame.Empty |]
        _entityManager.Initialize
        
    let addFrame (data: {| Entities:Map<uint32,AbstractComponent[]>; SCL:SystemChangeLog; GameEvents:AbstractGameEvent[] |}) =
        _frames <- Array.append _frames [|{ Number=(uint32 _frames.Length); Entities=data.Entities; SCL=data.SCL; GameEvents=data.GameEvents }|]
        Array.last _frames

    member private this.assignController =
        match Controller |> _entityManager.GetAllWithComponent with
        | [||] -> printf "no controller"; None
        | l -> printfn "controller on:%A" l.[0]; Some l.[0]
    member private this.gameLoop =
        let gel0 = _eventManager.ProcessEvents
        let scl = _systemManager.UpdateSystems
        let gel = _eventManager.ProcessEvents |> Array.append gel0
        _entityManager.SetCurrentToNext
        let f = addFrame {| Entities = _entityManager.Entities; SCL=scl; GameEvents = gel |}
        renderer _entityManager (int f.Number)
    
    member this.Frame_Current = Array.last _frames
    member this.EventManager = _eventManager
    member this.EntityManager = _entityManager
    member this.SystemManager = _systemManager

    member this.Start = 
        this.gameLoop

        printfn "#:%i" _entityManager.Entities.Count

        this.assignController |> _input.SetEntityID
        
        while _input.AwaitKeyboardInput do
            this.gameLoop
            
            //let gel = _eventManager.ProcessEvents
            
            //let (newecd,scl) = _systemManager.UpdateSystems 

            //let f = addFrame {| ECD = newecd; SCL=scl; GameEvents = gel |}
            
            //renderer f

