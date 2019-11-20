module agent_ComponentTypes
open CommonGenericFunctions
open Component


type private agent_ComponentTypesMsg = 
    | Add of RoundNumber * ComponentTypeID * ComponentID
    | Get of ComponentTypeID * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
    //| GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
    //| RemoveComponent of Component

type agent_ComponentTypes() = 

    let agent_ComponentTypes =
        let mutable _map = Map.empty<ComponentTypeID,(RoundNumber*ComponentID[] option)[]>
        MailboxProcessor<agent_ComponentTypesMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,ctid,cid) ->
                            _map <-
                                match _map.ContainsKey(ctid) with
                                | false -> _map.Add(ctid,[|(round,Some [|cid|])|])
                                | true -> 
                                    let oldArray = _map.Item(ctid)
                                    let addNew =
                                        match (snd oldArray.[0]) with
                                        | None -> [|(round,Some [|cid|])|]
                                        | Some eids -> [|(round, Some (Array.append eids [|cid|]))|]
                                    _map.Remove(ctid).Add(ctid,Array.append addNew oldArray) 
                        | Get (ctid,replyChannel) -> 
                            replyChannel.Reply(
                                match _map.ContainsKey(ctid) with
                                | false -> Array.empty
                                | true -> _map.Item(ctid))
                        //| GetComponentMap replyChannel -> 
                        //    replyChannel.Reply(_map)
                        //| RemoveComponent ct ->
                        //    _map <- Map_RemoveValueFromArray _map ct.ComponentID ct.EntityID
                }
            )

    member _.Add round ctid cid = agent_ComponentTypes.Post (Add (round,ctid,cid))
    member _.Get round ctid = agent_ComponentTypes.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) |> searchArrayDataForRound round
