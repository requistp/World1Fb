module agent_LocationEntity
open Component
open CommonGenericFunctions
open LocationTypes


type private agent_LocationEntityMsg =
| Add of FormComponent
| Get of LocationDataInt * AsyncReplyChannel<uint32[]>
| Init of Map<LocationDataInt,uint32[]>    
| Remove of FormComponent


type agent_LocationEntity() = 
    let agent =
        let mutable _map = MapLocations |> Array.fold (fun (m:Map<LocationDataInt,uint32[]>) l -> m.Add(l,Array.empty)) Map.empty
        MailboxProcessor<agent_LocationEntityMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add fd ->
                            _map <- Map_AppendValueToArrayUnique _map fd.Location fd.EntityID 
                        | Get (location,replyChannel) -> 
                            replyChannel.Reply(_map.Item(location))
                        | Init newMap -> 
                            _map <- newMap 
                        | Remove fd ->
                            _map <- Map_RemoveValueFromArray _map fd.Location fd.EntityID
                }
            )

    member _.Add (fd:FormComponent) = agent.Post (Add fd)

    member me.Add (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponent.ID)
        |> Array.Parallel.iter (fun ct -> me.Add ct.ToForm)

    member _.Get (location:LocationDataInt) = agent.PostAndReply (fun replyChannel -> Get (location,replyChannel))

    member _.Init (newMap:Map<LocationDataInt,uint32[]>) = agent.Post (Init newMap)

    member me.Move (oldForm:FormComponent,newForm:FormComponent) =
        me.Remove oldForm 
        me.Add newForm 

    member _.PendingUpdates = agent.CurrentQueueLength > 0

    member _.Remove (fd:FormComponent) = agent.Post (Remove fd)

    member me.Remove (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponent.ID)
        |> Array.Parallel.iter (fun ct -> me.Remove ct.ToForm)


