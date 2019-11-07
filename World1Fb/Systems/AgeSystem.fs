module AgeSystem
open GameManager
open InterruptTypes
open LocationTypes
open SystemManager


type AgeSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member me.interrupt_CanMate (interrupt:Interrupts) =
        System.Console.SetCursorPosition(0,MapHeight+1)
        printfn "in onCanMate check"
        false
        
    override _.ToString = "AgeSystem"

    override me.Initialize = 
        evm.RegisterInterrupt me.ToString Interrupt_CanMateID me.interrupt_CanMate
        base.SetToInitialized

    override me.Update = 
        ()


