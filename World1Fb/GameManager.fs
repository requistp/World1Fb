module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System
open InputHandler
open EventManager 

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : EntityComponentChange list
    abstract member Update : EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : EntityComponentChange list
    GameEvents : GameEvent[]
    }

type Game(ecd:EntityComponentData, renderer:Frame->unit) =
    let mutable _frames = List.empty:Frame list
    let mutable _systems = List.empty:AbstractSystem list
    let _eventManager = new EventManager()
    let _input = new InputHandler(_eventManager)

    do
        _frames <- [{ Number = 0u; ECD = ecd; ChangeLog = List.empty; GameEvents = Array.empty}]

    let addFrame (data: {| ECD:EntityComponentData; ChangeLog:EntityComponentChange list; GameEvents:GameEvent[] |}) =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=data.ECD; ChangeLog=data.ChangeLog; GameEvents=data.GameEvents } :: _frames
        _frames.Head

    let applyChangeLog (gel:GameEvent[]) eccl = 
        let mutable newecd = _frames.Head.ECD
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm 
        addFrame {| ECD = newecd; ChangeLog = eccl; GameEvents = gel |}

    let collectAndApplyChange filterFx processFx (gel:GameEvent[]) = 
        _systems
        |> List.filter filterFx
        |> List.collect processFx
        |> applyChangeLog gel

    member this.ECD = _frames.Head.ECD
    member this.EventManager = _eventManager

    member private this.Initialize = 
        collectAndApplyChange (fun s -> s.IsActive) (fun s -> s.Initialize) (Array.empty<GameEvent>)

    member private this.Update gel =
        collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update) gel
    
    member _.RegisterSystems (sl:AbstractSystem list) =
        _systems <- sl

    member this.Start = 
        renderer this.Initialize 

        while _input.AwaitKeyboardInput do
            let gel = _eventManager.ProcessEvents
            let f = (this.Update gel)
            renderer f
