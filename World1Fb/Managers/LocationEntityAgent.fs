module LocationEntityAgent
open AbstractComponent
open EntityAgent
open FormComponent
open LocationTypes


type LocationEntityAgentMsg =
    | Add of FormComponent
    | Get of LocationDataInt * AsyncReplyChannel<uint32[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
    | Init of Map<LocationDataInt,uint32[]>    
    | Remove of FormComponent


type LocationEntityAgent() = 

    let agent =
        let mutable _locations = 
            MapLocations 
            |> Array.fold (fun (m:Map<LocationDataInt,EntityAgent>) l -> m.Add(l,new EntityAgent())) Map.empty

        MailboxProcessor<LocationEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add f ->
                            _locations.Item(f.Location).Add f.EntityID
                        | Get (location,replyChannel) -> 
                            replyChannel.Reply(_locations.Item(location).Get)
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_locations |> Map.map (fun k v -> v.Get))
                        | Init newMap -> 
                            _locations <- 
                                newMap 
                                |> Map.fold (fun m k v -> m.Add(k,(new EntityAgent(v)))) Map.empty<LocationDataInt,EntityAgent>
                        | Remove f ->
                            _locations.Item(f.Location).Remove f.EntityID
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

    member this.Print() =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.Print) v.Length)
        
    member this.Remove (form:FormComponent) =
        agent.Post (Remove form)

    member this.Remove (cts:AbstractComponent[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentType = Component_Form)
        |> Array.iter (fun ct -> this.Remove (ct:?>FormComponent))
        

