module AbstractComponent
open System

type ComponentTypes =
    | Component_Eating      //= 0
    | Component_Food        //= 1
    | Component_Form        //= 2
    | Component_Controller  //= 3
    | Component_Movement    //= 4
    | Component_PlantGrowth //= 5
    | Component_Terrain     //= 6

let ComponentTypesAsArray = 
    [|
        Component_Eating      //= 0
        Component_Food        //= 1
        Component_Form        //= 2
        Component_Controller  //= 3
        Component_Movement    //= 4
        Component_PlantGrowth //= 5
        Component_Terrain     //= 6
    |]

[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid

    abstract member Copy : uint32 -> AbstractComponent

    member this.Abstract = this :> AbstractComponent

//type MsgEntityArray = 
//    | Add of uint32
//    | Remove of uint32
//    | List of AsyncReplyChannel<uint32[]>

//type EntityArray() =
//    let listAgent =
//        let mutable _entities = Array.empty<uint32>
//        let add eid =
//            match _entities |> Array.exists (fun e -> e = eid) with 
//            | true -> ()
//            | false -> _entities <- _entities |> Array.append [|eid|]
//        let remove eid =
//            match _entities |> Array.exists (fun e -> e = eid) with 
//            | false -> ()
//            | true -> _entities <- _entities |> Array.filter (fun e -> e <> eid)
//        MailboxProcessor<MsgEntityArray>.Start(
//            fun inbox ->
//                async 
//                    { 
//                        while true do
//                            let! msg = inbox.Receive()
//                            match msg with
//                            | Add eid -> add eid
//                            | Remove eid -> remove eid
//                            | List replyChannel  -> replyChannel.Reply(_entities)
//                    }
//            )
//    member this.Add eid = listAgent.Post (Add eid)
//    member this.Remove eid = listAgent.Post (Remove eid)
//    member this.List = listAgent.PostAndReply List 