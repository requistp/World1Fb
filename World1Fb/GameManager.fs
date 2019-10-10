module GameManager
open AbstractComponent
open CommonGenericFunctions
open EntityComponentManager
open EventManager
open GameEvents
open InputHandler
open System

type SystemChangeLog = 
    {
        Items : AbstractComponentChange[]
        Sum : AbstractComponentChange[]
    } with 
    static member empty = 
        { 
            Items = Array.empty
            Sum = Array.empty
        }
    static member New (c:'C,s:'S) =
        {   
            Items = c |> Array.Parallel.map (fun x -> x :> AbstractComponentChange)
            Sum = s |> Array.Parallel.map (fun x -> x :> AbstractComponentChange)
        }  
    member this.Add scl2 = 
        {
            Items = scl2.Items |> Array.append this.Items
            Sum = scl2.Sum |> Array.append this.Sum
        }

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    abstract member Update : EntityComponentData * SystemChangeLog -> (EntityComponentData * SystemChangeLog)

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
    let _input = new InputHandler(_eventManager) //, _frames.Head.ECD)

    do
        _frames <- [ {Frame.Empty with ECD=ecd} ] //Should just be empty frame after I change the the 0>1 frame step being the initial changes
        
    let addFrame (data: {| ECD:EntityComponentData; ChangeLog:AbstractComponentChange[]; SumOfChanges:AbstractComponentChange[]; GameEvents:AbstractGameEvent[] |}) =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=data.ECD; ChangeLog=data.ChangeLog; SumOfChanges=data.SumOfChanges; GameEvents=data.GameEvents } :: _frames
        _frames.Head

    let assignController =
        match Controller |> Entity.AllWithComponent _frames.Head.ECD with
        | [] -> None
        | l -> Some l.Head

    member _.ECD = _frames.Head.ECD
    member _.EventManager = _eventManager
    member _.Systems_Active = _systems |> List.filter (fun s -> s.IsActive)
    member this.Systems_ActiveAndInitialized = this.Systems_Active |> List.filter (fun s -> s.IsInitialized)

    member this.RegisterSystems (sl:AbstractSystem list) =
        _systems <- sl
        this.Systems_Active |> List.iter (fun s -> s.Initialize)

    member this.Start = 
        let mutable z = 0
        renderer _frames.Head

        assignController |> _input.SetEntityID

        while _input.AwaitKeyboardInput do
            let gel = _eventManager.ProcessEvents
            let (newecd,scl) = this.Systems_ActiveAndInitialized |> List.fold (fun d s -> s.Update d) (_frames.Head.ECD, SystemChangeLog.empty)
            let f = addFrame {| ECD = newecd; ChangeLog = scl.Items; SumOfChanges = scl.Sum; GameEvents = gel |}
            renderer f



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
    //member private this.CompilePendingChanges =
    //    this.Systems_ActiveAndInitialized |> List.fold (fun (scl:SystemChangeLog) sys -> scl.Add(sys.CompilePendingChanges)) SystemChangeLog.empty 
    //    //Switch this to a two step process with the first step being done in parallele after switching systems list to array

        //let cl = this.CompilePendingChanges
        //collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update) gel