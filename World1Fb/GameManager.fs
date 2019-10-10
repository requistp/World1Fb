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
        ECD : EntityComponentData
        SCL : SystemChangeLog
        GameEvents : AbstractGameEvent[]
    } with 
    static member Empty = 
        { 
            Number = 0u
            ECD = EntityComponentData.Empty
            SCL = SystemChangeLog.empty
            GameEvents = Array.empty<AbstractGameEvent>
        }

type Game(renderer:Frame->unit) =
    let mutable _frames = Array.empty:Frame[]
    let _eventManager = new EventManager()
    let _systemManager = new SystemManager()
    let _input = new InputHandler(_eventManager)

    do
        _frames <- [| Frame.Empty |]
        
    let addFrame (data: {| ECD:EntityComponentData; SCL:SystemChangeLog; GameEvents:AbstractGameEvent[] |}) =
        _frames <- Array.append _frames [|{ Number=(uint32 _frames.Length); ECD=data.ECD; SCL=data.SCL; GameEvents=data.GameEvents }|]
        Array.last _frames

    member private this.assignController =
        match Controller |> Entity.AllWithComponent (Array.last _frames).ECD with
        | [||] -> None
        | l -> Some l.[0]
    
    member this.Frame_Current = Array.last _frames
    member this.EventManager = _eventManager
    member this.SystemManager = _systemManager

    member this.Start = 
        let gel0 = _eventManager.ProcessEvents
        let (newecd0,scl0) = _systemManager.UpdateSystems this.Frame_Current.ECD
        let f0 = addFrame {| ECD = newecd0; SCL=scl0; GameEvents = gel0 |}
        renderer f0

        printfn "#:%i" this.Frame_Current.ECD.Entities.Count

        this.assignController |> _input.SetEntityID

        while _input.AwaitKeyboardInput do
            let gel = _eventManager.ProcessEvents
            
            let (newecd,scl) = _systemManager.UpdateSystems this.Frame_Current.ECD

            let f = addFrame {| ECD = newecd; SCL=scl; GameEvents = gel |}
            
            renderer f

