module EntityComponentAgent
open AbstractComponent
open ComponentEntityAgent
open EntityIDAgent
open FormComponent
open LocationEntityAgent
open LocationTypes

type private EntityComponentAgentMsg = 
| AddEntity of AbstractComponent[]
| Exists of uint32 * AsyncReplyChannel<bool>
| GetComponents of uint32 * AsyncReplyChannel<AbstractComponent[]>
| GetMap of AsyncReplyChannel< Map<uint32,AbstractComponent[]> >
| Init of Map<uint32,AbstractComponent[]>
| RemoveEntity of uint32
| ReplaceComponent of AbstractComponent

type EntityComponentAgent() = 
    let agentForID = new EntityIDAgent()
    let agentForComponents = new ComponentEntityAgent()
    let agentForLocations = new LocationEntityAgent()

    let agentForEntities =
        let mutable _next = Map.empty<uint32,AbstractComponent[]>
        MailboxProcessor<EntityComponentAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntity cts ->
                            if not (_next.ContainsKey(cts.[0].EntityID)) then 
                                _next <- _next.Add(cts.[0].EntityID, cts)
                        | Exists (eid,replyChannel) ->
                            replyChannel.Reply(_next.ContainsKey eid)
                        | GetComponents (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _next.ContainsKey eid with
                                | false -> [||]
                                | true -> _next.Item(eid)
                            )
                        | GetMap replyChannel ->
                            replyChannel.Reply(_next)
                        | Init newMap -> 
                            _next <- newMap
                        | RemoveEntity eid -> 
                            _next <- _next.Remove eid
                        | ReplaceComponent c ->
                            if (_next.ContainsKey c.EntityID) then
                                _next <-
                                    let a = 
                                        _next.Item(c.EntityID)
                                        |> Array.filter (fun ac -> ac.ComponentType <> ac.ComponentType)
                                        |> Array.append [|c|]
                                    _next.Remove(c.EntityID).Add(c.EntityID,a)
                }
            )

    member this.CreateEntity (cts:AbstractComponent[]) = 
        agentForComponents.Add cts
        agentForLocations.Add cts
        agentForEntities.Post (AddEntity cts)

    member this.Exists (eid:uint32) = 
        agentForEntities.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member this.GetComponents (eid:uint32) = 
        agentForEntities.PostAndReply (fun replyChannel -> GetComponents(eid,replyChannel))

    member this.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T =
        (this.GetComponents eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T
    
    member this.GetLocation (location:LocationDataInt) =
        agentForLocations.Get location

    member this.GetMap() = 
        agentForEntities.PostAndReply GetMap

    member this.GetMaxID = 
        agentForID.GetMaxID

    member this.GetNewID =
        agentForID.GetNewID
    
    member this.GetWithComponent ct =
        agentForComponents.Get ct
    
    member this.PendingUpdates = 
        (agentForEntities.CurrentQueueLength > 0) || agentForID.PendingUpdates || agentForComponents.PendingUpdates || agentForLocations.PendingUpdates

    member this.RemoveEntity (eid:uint32) = 
        let cts = this.GetComponents eid
        agentForComponents.Remove cts
        agentForLocations.Remove cts
        agentForEntities.Post (RemoveEntity eid)

    member this.ReplaceComponent (ac:AbstractComponent) =
        if (ac.ComponentType = Component_Form) then 
            agentForLocations.Move (this.GetComponent<FormComponent> ac.EntityID) (ac :?> FormComponent)
        agentForEntities.Post (ReplaceComponent ac)

    member this.Init (startMax:uint32) (newMap:Map<uint32,AbstractComponent[]>) = 
        agentForID.Init startMax
        agentForEntities.Post (Init newMap)


//member this.List () =
//    _entDict |> Map.iter (fun k v -> printfn "%i | %i" k v.List.Length)
       


