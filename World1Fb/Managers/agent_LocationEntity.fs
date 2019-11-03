module agent_LocationEntity
open Component
open CommonGenericFunctions
open LocationTypes


type private agent_LocationEntityMsg =
    | Add of FormComponent
    | Get of LocationDataInt * AsyncReplyChannel<uint32[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
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
                            if not (_map.Item(fd.Location) |> Array.contains fd.EntityID) then
                                _map <- Map_AppendValueToArray _map fd.Location fd.EntityID
                        | Get (location,replyChannel) -> 
                            replyChannel.Reply(_map.Item(location))
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | Init newMap -> 
                            _map <- newMap 
                        | Remove fd ->
                            _map <-
                                let a = _map.Item(fd.Location) |> Array.filter (fun eid -> eid <> fd.EntityID)
                                _map.Remove(fd.Location).Add(fd.Location,a)
                }
            )

    member this.Add (fd:FormComponent) =
        agent.Post (Add fd)

    member this.Add (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponent.ID)
        |> Array.iter (fun ct -> 
            let (Form fd)=ct
            this.Add fd)

    member this.Get (location:LocationDataInt) =
        agent.PostAndReply (fun replyChannel -> Get (location,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap

    member this.Init (newMap:Map<LocationDataInt,uint32[]>) =
        agent.Post (Init newMap)

    member this.Move (oldForm:FormComponent,newForm:FormComponent) =
        this.Remove oldForm 
        this.Add newForm 

    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member this.Print() =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.ToString()) v.Length)
        
    member this.Remove (fd:FormComponent) =
        agent.Post (Remove fd)

    member this.Remove (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponent.ID)
        |> Array.iter (fun ct -> 
            let (Form fd)=ct
            this.Remove fd)


