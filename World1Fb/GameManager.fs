module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityComponentManager
open EventManager
open GameEvents
open InputHandler
open System

type SystemChanges = 
    {
        ChangeLog : AbstractComponentChange[]
        SumOfChanges : AbstractComponentChange[]
    }

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    abstract member CompilePendingChanges : SystemChanges
    abstract member Update : unit

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
    let mutable _frames = List.empty:Frame list
    let mutable _systems = List.empty:AbstractSystem list
    let _eventManager = new EventManager()
    let _input = new InputHandler(_eventManager, _frames.Head.ECD)

    do
        _frames <- [ Frame.Empty ]

    let addFrame (data: {| ECD:EntityComponentData; ChangeLog:AbstractComponentChange[]; SumOfChanges:AbstractComponentChange[]; GameEvents:AbstractGameEvent[] |}) =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=data.ECD; ChangeLog=data.ChangeLog; SumOfChanges=data.SumOfChanges; GameEvents=data.GameEvents } :: _frames
        _frames.Head

    //let applyChangeLog (gel:GameEvent[]) eccl = 
    //    let mutable newecd = _frames.Head.ECD
    //    for ecc in eccl do 
    //        match ecc with
    //        | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
    //        | EntityRemoval e -> newecd <- Entity.Remove newecd e
    //        | _ -> () //newecm 
    //    addFrame {| ECD = newecd; ChangeLog = eccl; GameEvents = gel |}

    //let collectAndApplyChange filterFx processFx (gel:GameEvent[]) = 
    //    _systems
    //    |> List.filter filterFx
    //    |> List.collect processFx
    //    |> applyChangeLog gel

    member this.ECD = _frames.Head.ECD
    member this.EventManager = _eventManager

    member private this.Initialize = 
        _systems
        |> List.filter (fun s -> s.IsActive)
        |> List.iter (fun s -> s.Initialize)

    member private this.CompilePendingChanges gel =
        collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.CompilePendingChanges) gel

    member private this.Update gel =
        collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update) gel
    
    member _.RegisterSystems (sl:AbstractSystem list) =
        _systems <- sl

    member this.Start = 
        renderer _frames.Head

        while _input.AwaitKeyboardInput do
            let gel = _eventManager.ProcessEvents

            let f = (this.Update gel)
            renderer f
