module agent_Locations
open agent_Components
open CommonGenericFunctions
open Component
open FormComponent
open LocationTypes

type private agent_CurrentMsg =
    | Add of FormComponent
    | Get of LocationDataInt * AsyncReplyChannel<ComponentID[]>
    | GetMap of AsyncReplyChannel<Map<LocationDataInt,ComponentID[]> >
    | Init of Map<LocationDataInt,ComponentID[]>
    | Move of oldForm:FormComponent * newForm:FormComponent
    | Remove of FormComponent

type private agent_HistoryMsg =
    | History_Add of RoundNumber * FormComponent
    | History_Init of Map<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
    | History_Move of RoundNumber * oldForm:FormComponent * newForm:FormComponent
    | History_Remove of RoundNumber * FormComponent

type agent_Locations(useHistory:bool, compMan:agent_Components) = 
    let mutable _history = Map.empty<LocationDataInt,(RoundNumber*ComponentID[] option)[]>

    let getHistory round location = 
        match (_history.ContainsKey location) with
        | false -> [||]
        | true -> 
            match searchArrayDataForRound round (_history.Item location) with
            | None -> [||]
            | Some a -> a

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
                        | GetMap replyChannel -> replyChannel.Reply(_map)
                        | Init startMap -> _map <- startMap
                        | Move (oldForm,newForm) -> 
                            remove oldForm
                            add newForm
                        | Remove form -> remove form
                }
            )
            
    let agent_History =
        MailboxProcessor<agent_HistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add round (f:FormComponent) = 
                            _history <- 
                                match _history.ContainsKey f.Location with
                                | false -> _history.Add(f.Location,[|round,Some [|f.ID|]|])
                                | true -> 
                                    let h,t = _history.Item(f.Location) |> Array.splitAt 1
                                    let newArray = 
                                        match ((fst h.[0]) = round) with
                                        | false -> Array.append [|round,Some [|f.ID|]|] t
                                        | true -> Array.append [|round,Some [|f.ID|]|] (_history.Item f.Location)
                                    _history.Remove(f.Location).Add(f.Location,newArray)
                        let remove round (f:FormComponent) =
                            if (_history.ContainsKey f.Location) then
                                match snd (_history.Item f.Location).[0] with
                                | None -> ()
                                | Some a ->
                                    if a |> Array.contains f.ID then
                                        _history <-
                                            match a |> Array.filter (fun id -> id <> f.ID) with
                                            | [||] -> _history.Add(f.Location,[|round,None|])
                                            | n -> _history.Add(f.Location,[|round,Some n|])
                        match msg with
                        | History_Add (round,f) -> add round f
                        | History_Init startMap -> _history <- startMap
                        | History_Move (round,oldForm,newForm) -> 
                            remove round oldForm
                            add round newForm
                        | History_Remove (round,f) -> remove round f
                }
            )
    
    member _.Add (round:RoundNumber) (form:FormComponent) = 
        Async.Parallel
        (
            agent_Current.Post (Add form)
            if useHistory then agent_History.Post (History_Add (round,form))
        )
    member _.Get (round:RoundNumber option) location = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> Get (location,replyChannel))
        | Some r,true -> getHistory r location
        |> compMan.GetMany round
        |> Array.Parallel.map ToForm
    member _.GetForSave =
        agent_Current.PostAndReply GetMap
        ,
        _history
    member _.GetMap (round:RoundNumber option) = 
        match round,useHistory with
        | None,_ | Some _,false -> 
            agent_Current.PostAndReply GetMap
            |> Map.map (fun _ cids -> cids |> Array.choose (compMan.Get round) |> Array.map ToForm)
        | Some _,true -> 
            _history
            |> Map.map (fun _ a -> 
                match (snd a.[0]) with
                | None -> [||]
                | Some cids ->
                    cids |> Array.choose (compMan.Get round) |> Array.map ToForm
                )
    member _.Init currentMap historyMap =
        Async.Parallel
        (
            agent_Current.Post (Init currentMap)
            agent_History.Post (History_Init historyMap)
        )
    member _.Move (round:RoundNumber) oldForm newForm = 
        Async.Parallel
        (
            agent_Current.Post (Move (oldForm,newForm))
            if useHistory then agent_History.Post (History_Move (round,oldForm,newForm))
        )
    member _.Remove (round:RoundNumber) (form:FormComponent) = 
        Async.Parallel
        (
            agent_Current.Post (Remove form)
            if useHistory then agent_History.Post (History_Remove (round,form))
        )



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


