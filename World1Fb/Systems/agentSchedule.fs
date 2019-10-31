module agentSchedule
open CalendarTimings
open CommonGenericFunctions
open EventTypes


type private agentScheduleMsg =
| Add of ScheduledEvent
| Get of AsyncReplyChannel<ScheduledEvent[]>
| List of AsyncReplyChannel<Map<uint32,ScheduledEvent[]> >
| Reschedule of ScheduledEvent


type agentSchedule(getRound:unit->uint32) =
    let agent =
        let mutable _schedule = Map.empty<uint32,ScheduledEvent[]>
        MailboxProcessor<agentScheduleMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add se -> 
                            let offset = uint32 (TimingOffset (int se.Frequency))
                            _schedule <- Map_AppendValueToArray _schedule (getRound()+offset) se
                        | Get replyChannel -> 
                            let r = getRound()
                            replyChannel.Reply(
                                match _schedule.ContainsKey(r) with
                                | false -> [||]
                                | true -> _schedule.Item(r)
                                )
                            _schedule <- _schedule.Remove(r)
                        | List replyChannel ->
                            replyChannel.Reply(_schedule)
                        | Reschedule se -> 
                            _schedule <- Map_AppendValueToArray _schedule (getRound()+se.Frequency) se
                }
            )

    member this.Add (se:ScheduledEvent) =
        agent.Post (Add se)

    member this.Get =
        agent.PostAndReply Get

    member this.List =
        agent.PostAndReply List

    member this.Reschedule (se:ScheduledEvent) =
        agent.Post (Reschedule se)

