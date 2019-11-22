module agent_Locations
open agent_Components
open CommonGenericFunctions
open Component
open FormComponent
open LocationTypes


//type private agent_HistoryMsg =
//    | AddHistory of RoundNumber * LocationDataInt * ComponentID
//    | GetHistory of LocationDataInt * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
//    //| GetLocationMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
//    | MoveHistory of RoundNumber * oldForm:FormComponent * newForm:FormComponent
//    | RemoveHistory of RoundNumber * LocationDataInt * ComponentID


type private agent_CurrentMsg =
    | Add of FormComponent
    | Get of LocationDataInt * AsyncReplyChannel<ComponentID[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,ComponentID[]> >
    | Move of oldForm:FormComponent * newForm:FormComponent
    | Remove of FormComponent
    

type agent_Locations(compMan:agent_Components) = 

    //let agent_History =
    //    let mutable _map = Map.empty<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
    //    MailboxProcessor<agent_HistoryMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    | AddHistory (round,location,cid) -> 
    //                        _map <-
    //                            match _map.ContainsKey(location) with
    //                            | false -> _map.Add(location,[|(round,Some [|cid|])|])
    //                            | true -> 
    //                                let oldArray = _map.Item(location)
    //                                let addNew =
    //                                    match (snd oldArray.[0]) with
    //                                    | None -> [|(round,Some [|cid|])|]
    //                                    | Some cids -> [|(round, Some (Array.append cids [|cid|]))|]
    //                                _map.Remove(location).Add(location,Array.append addNew oldArray)
    //                    | GetHistory (location,replyChannel) -> 
    //                        match _map.ContainsKey location with
    //                        | false -> replyChannel.Reply(Array.empty)
    //                        | true -> replyChannel.Reply(_map.Item location)
    //                    //| GetLocationMap replyChannel -> 
    //                    //    replyChannel.Reply(_map)
    //                    | MoveHistory (round,oldForm,newForm) -> 
    //                        // First, remove oldForm from its location
    //                        match _map.ContainsKey oldForm.Location with
    //                        | false -> ()
    //                        | true ->
    //                            let oldHistory = _map.Item(oldForm.Location)
    //                            match (snd oldHistory.[0]) with
    //                            | None -> ()
    //                            | Some x when x = [||] -> () // Shouldn't happen as this should have been None
    //                            | Some oldComponents -> 
    //                                let newOldComponents = 
    //                                    match oldComponents |> Array.filter (fun cid -> cid <> oldForm.ID) with
    //                                    | [||] -> None
    //                                    | a -> Some a
    //                                _map <- _map.Remove(oldForm.Location).Add(oldForm.Location, Array.append [|(round,newOldComponents)|] oldHistory)
    //                        // Then Add newForm to new location
    //                        match _map.ContainsKey newForm.Location with
    //                        | false -> _map <- _map.Add(newForm.Location,[|(round,Some [|newForm.ID|])|])
    //                        | true ->
    //                            let newHistory = _map.Item(newForm.Location)
    //                            let newNewComponents = 
    //                                match (snd newHistory.[0]) with
    //                                | None -> Some [|newForm.ID|]
    //                                | Some x when x = [||] -> Some [|newForm.ID|] // Shouldn't happen as this should have been None
    //                                | Some oldNewComponents -> 
    //                                    Some (Array.append oldNewComponents [|newForm.ID|])
    //                            _map <- _map.Remove(newForm.Location).Add(newForm.Location, Array.append [|(round,newNewComponents)|] newHistory)
    //                    | RemoveHistory (round,location,cid) -> 
    //                        match _map.ContainsKey(location) with
    //                        | false -> ()
    //                        | true -> 
    //                            let history = _map.Item(location)
    //                            match (snd history.[0]) with 
    //                            | None -> ()
    //                            | Some a ->
    //                                match Array.contains cid a with 
    //                                | false -> ()
    //                                | true -> 
    //                                    let newArray = 
    //                                        match a |> Array.filter (fun c -> c <> cid) with 
    //                                        | [||] -> None
    //                                        | filtered -> Some filtered
    //                                    _map <- _map.Remove(location).Add(location,Array.append [|(round,newArray)|] history)
    //            }
    //        )
    
    let agent_Current =
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
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | Move (oldForm,newForm) -> 
                            remove oldForm
                            add newForm
                        | Remove form -> remove form
                }
            )
    member _.Add (round:RoundNumber) (form:FormComponent) = 
        agent_Current.Post (Add form)
        // FIX history
    member _.Get (round:RoundNumber option) location = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel)) // FIX history
        |> compMan.GetMany round
        |> Array.Parallel.map ToForm
    member _.GetIDs (round:RoundNumber option) location = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel)) // FIX history
    member _.GetMapIDs (round:RoundNumber option) = 
        match round with
        | None -> agent_Current.PostAndReply GetMap
        | Some r -> agent_Current.PostAndReply GetMap
    member me.GetMap (round:RoundNumber option) = 
        me.GetMapIDs round
        |> Map.map (fun _ cids -> cids |> Array.choose (compMan.Get round) |> Array.map ToForm)
    member _.Move (round:RoundNumber) oldForm newForm = 
        agent_Current.Post (Move (oldForm,newForm))
        // FIX history
    member _.Remove (round:RoundNumber) (form:FormComponent) = 
        agent_Current.Post (Remove form)
        // FIX history

//member _.Add round (form:FormComponent) = agent_History.Post (AddHistory (round,form.Location,form.ID))
//member _.Get round location = agent_History.PostAndReply (fun replyChannel -> GetHistory (location,replyChannel)) |> searchArrayDataForRound round
//member _.Move round oldForm newForm = agent_History.Post (MoveHistory (round,oldForm,newForm))
//member _.Remove round (form:FormComponent) = agent_History.Post (RemoveHistory (round,form.Location,form.ID))
