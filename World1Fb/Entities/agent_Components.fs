module agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component


type private agent_ComponentsMsg = 
    | Add of RoundNumber * Component
    | Get of ComponentID * AsyncReplyChannel<(RoundNumber*Component option)[]>
    | Init of Map<ComponentID,(RoundNumber*Component option)[]>
    | Remove of RoundNumber * ComponentID
    | Update of RoundNumber * Component


type agent_Components() =

    let idMan = new agent_IDManager()

    let agent_Components =
        let mutable _map = Map.empty<ComponentID,(RoundNumber*Component option)[]>
        MailboxProcessor<agent_ComponentsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,comp) ->
                            _map <- _map.Add(comp.ID,[|(round,Some comp)|])
                        | Get (cid,replyChannel) ->
                            replyChannel.Reply(
                                match _map.ContainsKey(cid) with
                                | false -> [||]
                                | true -> _map.Item(cid))
                        | Init startMap ->
                            _map <- startMap
                        | Remove (round,cid) ->
                            match _map.ContainsKey(cid) with
                            | false -> ()
                            | true ->
                                let a = _map.Item(cid)
                                match (snd a.[0]).IsNone with
                                | true -> () 
                                | false -> 
                                    _map <- _map.Remove(cid).Add(cid,Array.append [|(round,None)|] a)
                        | Update (round,comp) ->
                            match _map.ContainsKey(comp.ID) with
                            | false -> _map <- _map.Add(comp.ID,[|(round,Some comp)|])
                            | true ->
                                let a = _map.Item(comp.ID)
                                _map <- _map.Remove(comp.ID).Add(comp.ID,Array.append [|(round,Some comp)|] a)
                }
            )

    member _.Add round comp = agent_Components.Post (Add (round,comp))
    member _.Get round cid = agent_Components.PostAndReply (fun replyChannel -> Get (cid,replyChannel)) |> searchArrayDataForRound round
    member _.NewComponentID() = idMan.GetNewID()
    member _.Init (startMap:Map<ComponentID,(RoundNumber*Component option)[]>) =
        agent_Components.Post (Init startMap)
        idMan.Init (MapKeys startMap |>Seq.max)
    member _.Remove round cid = agent_Components.Post (Remove (round,cid))
    member _.Update round comp = agent_Components.Post (Update (round,comp))

