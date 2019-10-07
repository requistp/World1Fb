module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System
open InputHandler

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize: EntityComponentData -> EntityComponentChange list
    abstract member Update: EntityComponentData -> EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : EntityComponentChange list
    }

type Game(ecd:EntityComponentData, renderer:Frame->unit, systems:AbstractSystem list) =
    let mutable _frames = List.empty:Frame list
    let _input = new InputHandler()

    do
        _frames <- [{ Number=0u; ECD=ecd; ChangeLog=List.empty}]

    let addFrame (data: {| ECD:EntityComponentData; ChangeLog:EntityComponentChange list |}) =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=data.ECD; ChangeLog=data.ChangeLog} :: _frames
        _frames.Head

    let applyChangeLog eccl = 
        let mutable newecd = _frames.Head.ECD
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm 
        addFrame {| ECD = newecd; ChangeLog = eccl |}

    let collectAndApplyChange filterFx processFx = 
        systems
        |> List.filter filterFx
        |> List.collect processFx
        |> applyChangeLog

    member private this.Initialize = 
        collectAndApplyChange (fun s -> s.IsActive) (fun s -> s.Initialize _frames.Head.ECD)

    member private this.Update =
        collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update _frames.Head.ECD)
    
    member this.Start = 
        renderer this.Initialize

        while _input.AwaitKeyboardInput do
            renderer this.Update

