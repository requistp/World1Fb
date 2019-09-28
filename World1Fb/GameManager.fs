module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract
open SystemManager

type Frame(number:uint32, ecm:EntityComponentManager, eccl:EntityComponentChange list) =
    static member New = Frame(0u, EntityComponentManager(), List.empty)
    member this.Number = number
    member this.EntityManager = ecm
    member this.Add (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = Frame(number + 1u, ecm, eccl)
     
type Game(frames:Frame list) =
    let mutable _frames = frames

    let frame_Add (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = _frames <- [_frames.Head.Add ecm eccl] @ frames; _frames.Head
    let systemManager = SystemManager()
    let genericUpdate (updateFunc:Result<ChangesAndNewECM,string>) =
        match updateFunc with
        | Error e -> Error e
        | Ok tup -> Ok (frame_Add (snd tup) (fst tup))

    member this.EntityManager = _frames.Head.EntityManager
    member this.Frames = _frames
    member this.Frame_Current = frames.Head

    member this.InitializeGame (sl:AbstractSystem list) = 
        _frames <- [Frame.New]
        genericUpdate (systemManager.RegisterSystems sl)
    member this.Update = 
        genericUpdate (systemManager.Update this.EntityManager)
    
    new() = Game([Frame.New])
