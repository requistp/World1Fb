module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityComponentManager
open EventManager
open GameEvents
open InputHandler
open SystemManager

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : AbstractComponentChange[]
    SumOfChanges : AbstractComponentChange[]
    GameEvents : AbstractGameEvent[]
    } with 
    static member Empty = 
        { 
            Number = 0u; 
            ECD = EntityComponentData.Empty; 
            ChangeLog = Array.empty; 
            SumOfChanges = Array.empty; 
            GameEvents = Array.empty<AbstractGameEvent>
        }

type Game(ecd:EntityComponentData, renderer:Frame->unit) =
    let mutable _frames = Array.empty:Frame[]
    let _eventManager = new EventManager()
    let _systemManager = new SystemManager()
    let _input = new InputHandler(_eventManager) //, _frames.Head.ECD)

    do
        _frames <- [| {Frame.Empty with ECD=ecd} |] //Should just be empty frame after I change the the 0>1 frame step being the initial changes
        
    let addFrame (data: {| ECD:EntityComponentData; ChangeLog:AbstractComponentChange[]; SumOfChanges:AbstractComponentChange[]; GameEvents:AbstractGameEvent[] |}) =
        _frames <- Array.append _frames [|{ Number=(uint32 _frames.Length); ECD=data.ECD; ChangeLog=data.ChangeLog; SumOfChanges=data.SumOfChanges; GameEvents=data.GameEvents }|]
        Array.last _frames

    let assignController =
        match Controller |> Entity.AllWithComponent (Array.last _frames).ECD with
        | [||] -> None
        | l -> Some l.[0]
    
    member this.Frame_Current = Array.last _frames
    member this.EventManager = _eventManager
    member this.SystemManager = _systemManager

    member this.Start = 
        _frames |> Array.last |> renderer

        assignController |> _input.SetEntityID

        while _input.AwaitKeyboardInput do
            let gel = _eventManager.ProcessEvents
            
            let (newecd,scl) = _systemManager.UpdateSystems this.Frame_Current.ECD

            let f = addFrame {| ECD = newecd; ChangeLog = scl.Items; SumOfChanges = scl.Sum; GameEvents = gel |}
            
            renderer f

