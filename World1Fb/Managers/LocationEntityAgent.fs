module LocationEntityAgent
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes


type private LocationEntityAgentMsg =
    | Add of eintityID:uint32 * FormData
    | Get of LocationDataInt * AsyncReplyChannel<uint32[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
    | Init of Map<LocationDataInt,uint32[]>    
    | Remove of eintityID:uint32 * FormData


type LocationEntityAgent() = 
    let agent =
        let mutable _map = MapLocations |> Array.fold (fun (m:Map<LocationDataInt,uint32[]>) l -> m.Add(l,Array.empty)) Map.empty
        MailboxProcessor<LocationEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (e,fd) ->
                            if not (_map.Item(fd.Location) |> Array.contains e) then
                                _map <- Map_AppendValueToArray _map fd.Location e
                        | Get (location,replyChannel) -> 
                            replyChannel.Reply(_map.Item(location))
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | Init newMap -> 
                            _map <- newMap 
                        | Remove (e,fd) ->
                            _map <-
                                let a = _map.Item(fd.Location) |> Array.filter (fun eid -> eid <> e)
                                _map.Remove(fd.Location).Add(fd.Location,a)
                }
            )

    member this.Add (e,form:FormData) =
        agent.Post (Add (e,form))

    member this.Add (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = 1)
        |> Array.iter (fun ct -> 
            let (Form (e,fd))=ct
            this.Add (e,fd))

    member this.Get (location:LocationDataInt) =
        agent.PostAndReply (fun replyChannel -> Get (location,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap

    member this.Init (newMap:Map<LocationDataInt,uint32[]>) =
        agent.Post (Init newMap)

    member this.Move (e,oldForm:FormData,newForm:FormData) = //(oldForm:Component.Form) (newForm:Component.Form) =
        this.Remove (e,oldForm) //oldForm
        this.Add (e,newForm) //newForm

    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member this.Print() =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.Print) v.Length)
        
    member this.Remove (e,fd:FormData) =
        agent.Post (Remove (e,fd))

    member this.Remove (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = 1)
        |> Array.iter (fun ct -> 
            let (Form (e,fd))=ct
            this.Remove (e,fd))
//        this.Remove (ct) //:?>FormComponent))
        

