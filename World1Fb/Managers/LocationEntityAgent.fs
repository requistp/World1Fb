module LocationEntityAgent
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes


type private LocationEntityAgentMsg =
| Add of FormComponent
| Get of LocationDataInt * AsyncReplyChannel<uint32[]>
| GetMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
| Init of Map<LocationDataInt,uint32[]>    
| Remove of FormComponent


type LocationEntityAgent() = 
    let agent =
        let mutable _map = MapLocations |> Array.fold (fun (m:Map<LocationDataInt,uint32[]>) l -> m.Add(l,Array.empty)) Map.empty
        MailboxProcessor<LocationEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add f ->
                            if not (_map.Item(f.Location) |> Array.contains f.EntityID) then
                                _map <- Map_AppendValueToArray _map f.Location f.EntityID
                        | Get (location,replyChannel) -> 
                            replyChannel.Reply(_map.Item(location))
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | Init newMap -> 
                            _map <- newMap 
                        | Remove f ->
                            _map <-
                                let a = _map.Item(f.Location) |> Array.filter (fun eid -> eid <> f.EntityID)
                                _map.Remove(f.Location).Add(f.Location,a)
                }
            )

    member this.Add (form:FormComponent) =
        agent.Post (Add form)

    member this.Add (cts:AbstractComponent[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentType = Component_Form)
        |> Array.iter (fun ct -> this.Add (ct:?>FormComponent))

    member this.Get (location:LocationDataInt) =
        agent.PostAndReply (fun replyChannel -> Get (location,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap

    member this.Init (newMap:Map<LocationDataInt,uint32[]>) =
        agent.Post (Init newMap)

    member this.Move (oldForm:FormComponent) (newForm:FormComponent) =
        this.Remove oldForm
        this.Add newForm

    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member this.Print() =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.Print) v.Length)
        
    member this.Remove (form:FormComponent) =
        agent.Post (Remove form)

    member this.Remove (cts:AbstractComponent[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentType = Component_Form)
        |> Array.iter (fun ct -> this.Remove (ct:?>FormComponent))
        

