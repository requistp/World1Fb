module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract
open SystemManager

type Frame(number:uint32, ecd:EntityComponentData, eccl:EntityComponentChange list) =
    static member New = Frame(0u, EntityComponentData.New, List.empty)
    member this.Number = number
    member this.EntityComponentData = ecd
    member this.Add (ecd:EntityComponentData) (eccl:EntityComponentChange list) = Frame(number + 1u, ecd, eccl)
     
type Game(sl:AbstractSystem list, frames:Frame list) =
    let mutable _frames = frames
    let _systems = sl

    let genericUpdate (tup:ChangesAndNewECData) =
        _frames <- [_frames.Head.Add (snd tup) (fst tup)] @ frames
        _frames.Head

    member this.Frames = _frames
    member this.Frame_Current = _frames.Head
    member this.Systems = _systems

    member this.InitializeGame = 
        _frames <- [Frame.New]
        genericUpdate (SystemManager.RegisterSystems _systems)
    member this.Update = 
        genericUpdate (SystemManager.Update _systems this.Frame_Current.EntityComponentData)
    
    new(sl:AbstractSystem list) = Game(sl, [Frame.New])


//type Frame(number:uint32, ecm:EntityComponentManager, eccl:EntityComponentChange list) =
//    static member New = Frame(0u, EntityComponentManager(), List.empty)
//    member this.Number = number
//    member this.EntityManager = ecm
//    member this.Add (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = Frame(number + 1u, ecm, eccl)
     
//type Game(frames:Frame list) =
//    let mutable _frames = frames

//    let frame_Add (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = _frames <- [_frames.Head.Add ecm eccl] @ frames; _frames.Head
//    let systemManager = SystemManager()
//    let genericUpdate (updateFunc:Result<ChangesAndNewECM,string>) =
//        match updateFunc with
//        | Error e -> Error e
//        | Ok tup -> Ok (frame_Add (snd tup) (fst tup))

//    member this.EntityManager = _frames.Head.EntityManager
//    member this.Frames = _frames
//    member this.Frame_Current = frames.Head

//    member this.InitializeGame (sl:AbstractSystem list) = 
//        _frames <- [Frame.New]
//        genericUpdate (systemManager.RegisterSystems sl)
//    member this.Update = 
//        genericUpdate (systemManager.Update this.EntityManager)
    
//    new() = Game([Frame.New])
