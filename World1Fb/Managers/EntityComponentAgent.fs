module EntityComponentAgent
open AbstractComponent
open CommonGenericFunctions
open ComponentAgent


type EntityComponentAgentMsg = 
    | ComponentList of uint32 * AsyncReplyChannel<AbstractComponent[]>
    | CreateEntity of AbstractComponent[]
    | Exists of uint32 * AsyncReplyChannel<bool>
    | Init of Map<uint32,AbstractComponent[]>
    | GetMap of AsyncReplyChannel< Map<uint32,AbstractComponent[]> >
    | RemoveEntity of uint32
    | ReplaceComponent of AbstractComponent


type EntityComponentAgent() = 

    let agent =
        let mutable _entDict = Map.empty<uint32,ComponentAgent>

        MailboxProcessor<EntityComponentAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | ComponentList (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _entDict.ContainsKey eid with
                                | false -> [||]
                                | true -> _entDict.Item(eid).Get
                            )
                        | CreateEntity cts ->
                            if not (_entDict.ContainsKey(cts.[0].EntityID)) then 
                                _entDict <- _entDict.Add(cts.[0].EntityID, new ComponentAgent(cts))
                        | Exists (eid,replyChannel) ->
                            replyChannel.Reply(_entDict.ContainsKey eid)
                        | GetMap replyChannel ->
                            replyChannel.Reply(_entDict |> Map.map (fun k v -> v.Get))
                        | Init newMap -> 
                            _entDict <- 
                                newMap |> Map.map (fun k v -> new ComponentAgent(v))
                        | RemoveEntity eid -> 
                            _entDict <- _entDict.Remove eid
                        | ReplaceComponent ct ->
                            match _entDict.ContainsKey ct.EntityID with
                            | false -> ()
                            | true -> _entDict.Item(ct.EntityID).Replace ct
                }
            )

    member this.CopyEntity (oldeid:uint32) (neweid:uint32) =
        this.GetComponents oldeid
        |> Array.Parallel.map (fun (ct:AbstractComponent) -> ct.Copy neweid)

    member this.CreateEntity (cts:AbstractComponent[]) = 
        agent.Post (CreateEntity cts)

    member this.Exists (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member this.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T =
        (this.GetComponents eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T

    member this.GetComponents (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> ComponentList(eid,replyChannel))

    member this.GetMap() = 
        agent.PostAndReply GetMap

    member this.HasAllComponents (cts:'T[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> this.GetComponents eid |> Array.exists (fun ec -> ec.GetType() = ct))

    member this.RemoveEntity (eid:uint32) = 
        agent.Post (RemoveEntity eid)

    member this.ReplaceComponent (ct:AbstractComponent) =
        agent.Post (ReplaceComponent ct)

    member this.Init (newMap:Map<uint32,AbstractComponent[]>) = 
        agent.Post (Init newMap)

    member this.TryGet (eid:uint32) =
        this.Exists eid |> TrueSomeFalseNone (this.GetComponents eid)

    member this.TryGetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : Option<'T> = 
        match this.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)

    member this.TryGetComponentForEntities<'T when 'T:>AbstractComponent> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> this.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)


//member this.List () =
//    _entDict |> Map.iter (fun k v -> printfn "%i | %i" k v.List.Length)
       


