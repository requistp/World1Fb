module agent_EntityManager
open agent_IDManager
open CommonGenericFunctions


type private agent_EntitiesMsg = 
    | Add of RoundNumber * EntityID * ComponentID[]
    //| DeleteEntity of round:RoundNumber * entityID:uint32
    //| EntityExists of round:RoundNumber * entityID:uint32 * exists:AsyncReplyChannel<bool>
    | Get of EntityID * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
    //| UpdateComponents of round:RoundNumber * entityID:EntityID * compIDs:int[]
    //| GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
    //| InitEntities of Map<uint32,Component[]>
    //| ReplaceComponent of componentID:int * comp:Component


type agent_EntityManager() =

    let idMan = new agent_IDManager()

    let agent_Entities =
        let mutable _map = Map.empty<EntityID,(RoundNumber*ComponentID[] option)[]>
        MailboxProcessor<agent_EntitiesMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,eid,compIDs) ->
                            _map <- _map.Add(eid,[|(round,Some compIDs)|])
                        //| DeleteEntity (round,eid) -> 
                        //    if (_map.ContainsKey eid) then 
                        //        _map <- 
                        //            let array = _map.Item eid
                        //            match (snd array.[0]) with
                        //            | None -> _map
                        //            | Some _ -> 
                        //                let newArray = Array.append [|(round,None)|] array
                        //                _map.Remove(eid).Add(eid,newArray)
                        | Get (eid,replyChannel) ->
                            match _map.ContainsKey eid with
                            | false -> replyChannel.Reply(Array.empty)
                            | true -> replyChannel.Reply(_map.Item eid)
                        //| UpdateComponents (r,eid,compIDs) ->
                        //    match (snd _array.[eid].[0]).IsNone with
                        //    | true -> () //Should probably raise an error
                        //    | false ->
                        //        _array.[eid] <- Array.append [|(r,Some compIDs)|] _array.[eid]
                }
            )
    member _.Add round eid cids = agent_Entities.Post (Add (round,eid,cids))
    member _.Get round eid = agent_Entities.PostAndReply (fun replyChannel -> Get (eid,replyChannel)) |> searchArrayDataForRound round
    member _.NewEntityID() = idMan.GetNewID()


