module agent_Locations
open CommonGenericFunctions
open FormComponent
open LocationTypes


type private agent_LocationsMsg =
    | Add of RoundNumber * LocationDataInt * ComponentID
    | Get of LocationDataInt * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
    //| GetLocationMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
    //| RemoveForm of FormComponent
    | Move of RoundNumber * oldForm:FormComponent * newForm:FormComponent


type agent_Locations() = 

    let agent_Locations =
        let mutable _map = Map.empty<LocationDataInt,(RoundNumber*ComponentID[] option)[]>
        MailboxProcessor<agent_LocationsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add (round,location,cid) -> 
                            _map <-
                                match _map.ContainsKey(location) with
                                | false -> _map.Add(location,[|(round,Some [|cid|])|])
                                | true -> 
                                    let oldArray = _map.Item(location)
                                    let addNew =
                                        match (snd oldArray.[0]) with
                                        | None -> [|(round,Some [|cid|])|]
                                        | Some cids -> [|(round, Some (Array.append cids [|cid|]))|]
                                    _map.Remove(location).Add(location,Array.append addNew oldArray)
                        | Get (location,replyChannel) -> 
                            match _map.ContainsKey location with
                            | false -> replyChannel.Reply(Array.empty)
                            | true -> replyChannel.Reply(_map.Item location)
                        //| GetLocationMap replyChannel -> 
                        //    replyChannel.Reply(_map)
                        | Move (round,oldForm,newForm) -> 
                            // First, remove oldForm from its location
                            match _map.ContainsKey oldForm.Location with
                            | false -> ()
                            | true ->
                                let oldHistory = _map.Item(oldForm.Location)
                                match (snd oldHistory.[0]) with
                                | None -> ()
                                | Some x when x = [||] -> () // Shouldn't happen as this should have been None
                                | Some oldComponents -> 
                                    let newOldComponents = 
                                        match oldComponents |> Array.filter (fun cid -> cid <> oldForm.ID) with
                                        | [||] -> None
                                        | a -> Some a
                                    _map <- _map.Remove(oldForm.Location).Add(oldForm.Location, Array.append [|(round,newOldComponents)|] oldHistory)

                            // Then Add newForm to new location
                            match _map.ContainsKey newForm.Location with
                            | false -> _map <- _map.Add(newForm.Location,[|(round,Some [|newForm.ID|])|])
                            | true ->
                                let newHistory = _map.Item(newForm.Location)
                                let newNewComponents = 
                                    match (snd newHistory.[0]) with
                                    | None -> Some [|newForm.ID|]
                                    | Some x when x = [||] -> Some [|newForm.ID|] // Shouldn't happen as this should have been None
                                    | Some oldNewComponents -> 
                                        Some (Array.append oldNewComponents [|newForm.ID|])
                                _map <- _map.Remove(newForm.Location).Add(newForm.Location, Array.append [|(round,newNewComponents)|] newHistory)
                }
            )
    member _.Add round location cid = agent_Locations.Post (Add (round,location,cid))
    member _.Get round location = agent_Locations.PostAndReply (fun replyChannel -> Get (location,replyChannel)) |> searchArrayDataForRound round
    member _.Move round oldForm newForm = agent_Locations.Post (Move (round,oldForm,newForm))

