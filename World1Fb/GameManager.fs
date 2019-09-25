module GameManager
open CommonGenericFunctions
//open ECM2
open EntityComponentManager

[<Struct>]
type Frame(number:uint32, ecman:EntityComponentManager) =
    static member New = Frame(0u, EntityComponentManager())
    member this.Number = number
    member this.EntityManager = ecman
    member this.Add(ecman:EntityComponentManager) = Frame(number + 1u, ecman)

type Game(frames:Frame list) =
    let mutable _frames = frames
    let isInitialized = (_frames.Head.Number > 0u) 
    let frame_Add (ecman:EntityComponentManager) = _frames <- [_frames.Head.Add(ecman)] @ frames; _frames.Head
 
    member this.EntityManager = _frames.Head.EntityManager
    member this.Frames = _frames
    member this.Frame_Current = frames.Head
    member this.IsInitialized = isInitialized

    member this.InitializeGame (ecman:EntityComponentManager) = 
        match not isInitialized || ecman.Entities.IsEmpty with
        | true -> Failure "Game is not initialized"
        | false -> Success (frame_Add ecman)

    member this.Update ecman = 
        match isInitialized with
        | false -> Failure "Game is not initialized"
        | true  -> Success (frame_Add ecman)
        //Run all systems to generate the new state of all Entities (ie, it should return an ECM which would be piped in below)
        //  _frames.Head.ECM 
        //  |> SystemManager.Update 
        //  |> frame_Add
    
    new() = Game([Frame(0u, EntityComponentManager())])
