module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System

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

module Game =
    let mutable _frames = List.empty:Frame list

    let private addFrame c =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=fst c; ChangeLog=snd c} :: _frames
        _frames.Head

    let private applyChangeLog ecd eccl = 
        let mutable newecd = ecd
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm 
        addFrame (newecd,eccl)

    let private collectAndApplyChange filterFx processFx ecd (sl:AbstractSystem list) = 
        sl
        |> List.filter filterFx
        |> List.collect processFx
        |> applyChangeLog ecd

    let private Initialize ecd systems = 
        _frames <- [{ Number=0u; ECD=ecd; ChangeLog=List.empty}]
        // Frame 0 should be the initial world state before initialization
        // ...then we run Initialize and that is frame 1, return the list
        systems |> collectAndApplyChange (fun s -> s.IsActive) (fun s -> s.Initialize ecd) ecd

    let private Update ecd systems = 
        systems |> collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update ecd) ecd
        
    let Start (ecd:EntityComponentData) (renderer: Frame -> unit) (systems:AbstractSystem list) = 
        let mutable _abort = false

        renderer (Initialize ecd systems)

        while not _abort do
            while not Console.KeyAvailable do
                System.Threading.Thread.Sleep 250
        
            let k = Console.ReadKey(true).Key
    
            match k with
            | ConsoleKey.Escape -> _abort <- true
            | _ -> renderer (Update _frames.Head.ECD systems)