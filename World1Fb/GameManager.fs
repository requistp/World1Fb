module GameManager
open CommonGenericFunctions
open EntityComponentManager

[<Struct>]
type Frame(number:uint32, ecman:EntityComponentManager) =
    static member New = Frame(0u, EntityComponentManager())
    member this.Number = number
    member this.ECMan = ecman
    member this.Add(ecman:EntityComponentManager) = Frame(number + 1u, ecman)

type Game(frames:Frame list) =
    let mutable _frames = frames
    let isInitialized = (_frames.Head.Number > 0u)
    let frame_Add (ecman:EntityComponentManager) = _frames <- [_frames.Head.Add(ecman)] @ frames

    let InitializeGame (ecman:EntityComponentManager) = 
        match ecman.ECMap.Map.IsEmpty with
        | true -> _frames <- [Frame.New]
        | false -> frame_Add ecman
        _frames.Head
 
    member this.Frames = _frames
    member this.IsInitialized = isInitialized
    member this.Frame_Current = frames.Head

    member this.StartGame_New ecman = 
        match isInitialized with 
        | true -> Failure "Game is already started"
        | false -> Success (InitializeGame ecman) //Set base state

    member this.Update ecman = 
        match isInitialized with
        | false -> Failure "Game is not initialized"
        | true  -> Success (frame_Add ecman)
        //Run all systems to generate the new state of all Entities (ie, it should return an ECM which would be piped in below)
        //  _frames.Head.ECM 
        //  |> SystemManager.Update 
        //  |> frame_Add
    
    new() = Game([Frame.New])
    new(ecmap:EntityComponentData) =
        let f0 = Frame.New
        let ecman = EntityComponentManager ecmap
        Game([f0.Add(ecman)] @ [f0])
