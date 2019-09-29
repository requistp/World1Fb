module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open SystemManager

type Frame(number:uint32, ecd:EntityComponentData, eccl:EntityComponentChange list) =
    static member New = Frame(0u,EntityComponentData(Map.empty,0u),List.empty)
    member this.Number = number
    member this.EntityComponentData = ecd
    member this.ChangeLog = eccl
     
type Game(systems:AbstractSystem list, renderer:Frame -> unit) =
    let mutable _frames = [Frame.New]

    let newFrame ecd eccl = Frame(_frames.Head.Number + 1u, ecd, eccl)

    let asyncRender = async { renderer _frames.Head }

    let genericUpdate (tup:ChangesAndNewECData) =
        _frames <- (newFrame (snd tup) (fst tup)) :: _frames
        //Async.Start asyncRender
        renderer _frames.Head

    member this.InitializeGame = 
        _frames <- [Frame.New]
        genericUpdate (SystemManager.RegisterSystems systems)

    member this.Update = 
        genericUpdate (SystemManager.Update systems _frames.Head.EntityComponentData)
    

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
