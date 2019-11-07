module agent_InterruptChecker
open agent_GameEventLog
open CommonGenericFunctions
open InterruptTypes


type private agentEventInterruptMsg =
    | Check of round:uint32 * interrupt:Interrupts * result:AsyncReplyChannel<bool>
    | Register of round:uint32 * listener:string * interruptTypeID:byte * callback:InterruptCall


type agent_InterruptChecker(agentForLog:agent_GameEventLog) =
   
    let agent =        
        let mutable _interrupts = Map.empty<byte,(string*InterruptCall)[]>
        MailboxProcessor<agentEventInterruptMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Check (round,interrupt,replyChannel) ->
                            match _interrupts.ContainsKey interrupt.ID with
                            | false -> 
                                agentForLog.Log_NoInterruptListeners round interrupt
                                replyChannel.Reply(true)
                            | true ->
                                replyChannel.Reply(false)
                                //_interrupts.Item interrupt.ID
                                //|> Array.Parallel.iter (fun (listener,callback) -> agentForLog.Log_InterruptResult round (listener,callback,interrupt,callback interrupt))
                        | Register (round,listener,interruptTypeID,callback) -> 
                            _interrupts <- Map_AppendValueToArrayNonUnique _interrupts interruptTypeID (listener,callback)
                            agentForLog.Log_InterruptRegistered round listener
                }
            )

    member _.Check round interrupt = agent.PostAndReply (fun replyChannel -> Check(round,interrupt,replyChannel)) 

    member _.Register round listener interruptTypeID callback = agent.Post (Register (round,listener,interruptTypeID,callback))


