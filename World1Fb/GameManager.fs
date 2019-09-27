module GameManager
open ChangeLogManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract
open SystemManager

[<Struct>]
type Frame(number:uint32, ecm:EntityComponentManager, cl:ChangeLog) =
    static member New = Frame(0u, EntityComponentManager(), ChangeLog.New)
    member this.Number = number
    member this.EntityManager = ecm
    member this.Add (ecm:EntityComponentManager) (cl:ChangeLog) = Frame(number + 1u, ecm, cl)
     
type Game(frames:Frame list) =
    let mutable _frames = frames
    let frame_Add (ecm:EntityComponentManager) (cl:ChangeLog) = _frames <- [_frames.Head.Add ecm cl] @ frames; _frames.Head
    let systemManager = SystemManager()
    
    member this.EntityManager = _frames.Head.EntityManager
    member this.Frames = _frames
    member this.Frame_Current = frames.Head

    member this.InitializeGame (sl:AbstractSystem list) = 
        //Could check for game already initialized, but until that, I'm just gonna clear existing frams...
        _frames <- [Frame.New]
        match systemManager.RegisterSystems sl with
        | Error e -> Error e
        | Ok ecm -> Ok ecm//(frame_Add ecm)

    //member this.Update ecman = 
    //    match isInitialized with
    //    | false -> Failure "Game is not initialized"
    //    | true  -> Success (frame_Add ecman)
        //Run all systems to generate the new state of all Entities (ie, it should return an ECM which would be piped in below)
        //  _frames.Head.ECM 
        //  |> SystemManager.Update 
        //  |> frame_Add
    
    new() = Game([Frame.New])
