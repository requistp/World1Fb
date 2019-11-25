module agent_Locations
open agent_Components
open CommonGenericFunctions
open Component
open FormComponent
open LocationTypes

type Save_Locations = 
    {
        Locations_Current : Map<LocationDataInt,ComponentID[]>
    }

type private agent_CurrentMsg =
    | Add of FormComponent
    | Get of LocationDataInt * AsyncReplyChannel<ComponentID[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,ComponentID[]> >
    | Init of Map<LocationDataInt,ComponentID[]>
    | Move of oldForm:FormComponent * newForm:FormComponent
    | Remove of FormComponent

type agent_Locations(compMan:agent_Components) = 
    let agent =
        let mutable _map = Map.empty<LocationDataInt,ComponentID[]>
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add form = 
                            _map <-
                                match _map.ContainsKey form.Location with
                                | false -> _map.Add(form.Location,[|form.ID|])
                                | true ->
                                    let others = _map.Item(form.Location) |> Array.filter (fun c -> c <> form.ID) // In case component was already here
                                    _map.Remove(form.Location).Add(form.Location,Array.append others [|form.ID|]) 
                        let remove form =
                            if (_map.ContainsKey form.Location) then
                                let others = _map.Item(form.Location) |> Array.filter (fun c -> c <> form.ID)
                                _map <- _map.Remove(form.Location).Add(form.Location,others) 
                        match msg with
                        | Add form -> add form
                        | Get (location,replyChannel) -> 
                            match _map.ContainsKey location with
                            | false -> replyChannel.Reply(Array.empty)
                            | true -> replyChannel.Reply(_map.Item location)
                        | GetMap replyChannel -> replyChannel.Reply(_map)
                        | Init startMap -> _map <- startMap
                        | Move (oldForm,newForm) -> 
                            remove oldForm
                            add newForm
                        | Remove form -> remove form
                }
            )
            
    member _.Add (form:FormComponent) = agent.Post (Add form)
    member _.Get location = 
        agent.PostAndReply (fun replyChannel -> Get (location,replyChannel))
        |> compMan.GetMany
        |> Array.map ToForm
    member _.GetForSave = { Locations_Current = agent.PostAndReply GetMap }
    member _.GetMap = 
        agent.PostAndReply GetMap
        |> Map.map (fun _ cids -> cids |> compMan.GetMany |> Array.map ToForm) //Parallel slowed down the Array.map step
    member _.Init (save:Save_Locations) = agent.Post (Init save.Locations_Current)
    member _.Move oldForm newForm = agent.Post (Move (oldForm,newForm))
    member _.Remove (form:FormComponent) = agent.Post (Remove form)




//member _.Add round (form:FormComponent) = agent_History.Post (AddHistory (round,form.Location,form.ID))
//member _.Get round location = agent_History.PostAndReply (fun replyChannel -> GetHistory (location,replyChannel)) |> searchArrayDataForRound round
//member _.Move round oldForm newForm = agent_History.Post (MoveHistory (round,oldForm,newForm))
//member _.Remove round (form:FormComponent) = agent_History.Post (RemoveHistory (round,form.Location,form.ID))

//member _.GetIDs (round:RoundNumber option) location = 
//    match round with
//    | None -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel))
//    | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel)) // FIX history
//member _.GetMapIDs = 
//    match round with
//    | None -> agent_Current.PostAndReply GetMap
//    | Some r -> agent_Current.PostAndReply GetMap





//Locations_History : Map<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
//type private agent_HistoryMsg =
//    | History_Add of RoundNumber * FormComponent
//    | History_Init of Map<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
//    | History_Move of RoundNumber * oldForm:FormComponent * newForm:FormComponent
//    | History_Remove of RoundNumber * FormComponent
    //let mutable _history = Map.empty<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
    //let agent_History =
    //    MailboxProcessor<agent_HistoryMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    let add round (f:FormComponent) = 
    //                        _history <- 
    //                            match _history.ContainsKey f.Location with
    //                            | false -> _history.Add(f.Location,[|round,Some [|f.ID|]|])
    //                            | true -> 
    //                                let cids = 
    //                                    match (snd (_history.Item(f.Location).[0])) with
    //                                    | None -> [|f.ID|]
    //                                    | Some cids -> cids |> Array.filter (fun cid -> cid <> f.ID) |> Array.append [|f.ID|]
    //                                let newArray = Array.append [|round,Some cids|] (_history.Item f.Location)
    //                                _history.Remove(f.Location).Add(f.Location,newArray)
    //                    let remove round (f:FormComponent) =
    //                        _history <-
    //                            match (_history.ContainsKey f.Location) with
    //                            | false -> _history.Add(f.Location,[|round,None|])
    //                            | true ->
    //                                let cidso = 
    //                                    match (snd (_history.Item(f.Location).[0])) with
    //                                    | None -> None
    //                                    | Some cids -> Some (cids |> Array.filter (fun cid -> cid <> f.ID))
    //                                let newArray = Array.append [|round,cidso|] (_history.Item f.Location)
    //                                _history.Remove(f.Location).Add(f.Location,newArray)
    //                    match msg with
    //                    | History_Add (round,f) -> add round f
    //                    | History_Init startMap -> _history <- startMap
    //                    | History_Move (round,oldForm,newForm) -> 
    //                        remove round oldForm
    //                        add round newForm
    //                    | History_Remove (round,f) -> remove round f
    //            }
    //        )