module GameManager
open CommonGenericFunctions
open EntityComponentManager

type Frame = { number:int; ECM:EntityComponentManager }

type Game(frames:Frame list) =
    let mutable _frames = frames
    let isInitialized = (not _frames.IsEmpty) && (_frames.Head.number > 0)
    let frame_NextNumber = if isInitialized then _frames.Head.number + 1 else 1
    let frame_Next (ecm:EntityComponentManager) = { number = frame_NextNumber; ECM = ecm }
    let frame_Add (ecm:EntityComponentManager) = _frames <- [frame_Next ecm] @ frames; _frames

    let InitializeGame (ecm:EntityComponentManager) = 
        match ecm.EntitiesExist with
        | false -> _frames <- []; _frames
        | true -> frame_Add ecm
 
    member this.Frames = _frames
    member this.IsInitialized = isInitialized
    member this.Frame_Current = frames.Head

    member this.StartGame_New ecm = 
        match isInitialized with 
        | true -> Failure "Game is already started"
        | false -> Success (InitializeGame ecm) //Set base state

    member this.Update = 
        match isInitialized with
        | false -> Failure "Game is not initialized"
        | true  -> Success (frame_Add _frames.Head.ECM)
        //Run all systems to generate the new state of all Entities (ie, it should return an ECM which would be piped in below)
        //  _frames.Head.ECM 
        //  |> SystemManager.Update 
        //  |> frame_Add
    
    new(ecm:EntityComponentManager) = Game([{ number = 1; ECM = ecm }])
