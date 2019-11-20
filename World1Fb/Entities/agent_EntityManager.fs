module agent_EntityManager
open agent_IDManager
open CommonGenericFunctions


type private agent_EntitiesMsg = 
    | Add of RoundNumber * EntityID * ComponentID[]
    //| EntityExists of round:RoundNumber * entityID:uint32 * exists:AsyncReplyChannel<bool>
    | Get of EntityID * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
    //| UpdateComponents of round:RoundNumber * entityID:EntityID * compIDs:int[]
    //| GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
    //| InitEntities of Map<uint32,Component[]>
    | Remove of RoundNumber * EntityID
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
                        | Get (eid,replyChannel) ->
                            match _map.ContainsKey eid with
                            | false -> replyChannel.Reply(Array.empty)
                            | true -> replyChannel.Reply(_map.Item eid)
                        | Remove (round,eid) ->
                            match _map.ContainsKey(eid) with
                            | false -> ()
                            | true ->
                                let history =_map.Item(eid)
                                match (snd history.[0]) with
                                | None -> ()
                                | Some _ -> 
                                    _map <- _map.Remove(eid).Add(eid,Array.append [|(round,None)|] history)
                }
            )
    member _.Add round eid cids = agent_Entities.Post (Add (round,eid,cids))
    member _.Get round eid = agent_Entities.PostAndReply (fun replyChannel -> Get (eid,replyChannel)) |> searchArrayDataForRound round
    member _.NewEntityID() = idMan.GetNewID()
    member _.Remove round eid = agent_Entities.Post (Remove (round,eid))


