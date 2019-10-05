module GameManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize: EntityComponentData -> EntityComponentChange list
    abstract member Update: EntityComponentData -> EntityComponentChange list

type Frame = {
    Number : uint32
    ECD : EntityComponentData
    ChangeLog : EntityComponentChange list
    }

module Game =
    let mutable _frames = List.empty:Frame list

    let private addFrame c =
        _frames <- { Number=_frames.Head.Number + 1u; ECD=fst c; ChangeLog=snd c} :: _frames
        _frames

    let private applyChangeLog ecd eccl = 
        let mutable newecd = ecd
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm 
        addFrame (newecd,eccl)

    let private collectAndApplyChange filterFx processFx ecd (sl:AbstractSystem list) = 
        sl
        |> List.filter filterFx
        |> List.collect processFx
        |> applyChangeLog ecd
        
    let private handleKeyboardInput (kbState:KeyboardState) =
        //let rec HandleKeys keys =
        //    match keys with
        //    | x :: xs -> match x with
        //                 | _ -> true //HandleKeys xs
        //                 | Keys.Escape -> true
        //    | [] -> false
        
        let HandleKeys2 keys =
            printfn "%A" keys
            false

        HandleKeys2 (kbState.GetPressedKeys() |> Array.toList)

    let Initialize ecd systems = 
        _frames <- [{ Number=0u; ECD=ecd; ChangeLog=List.empty}]
        // Frame 0 should be the initial world state before initialization
        // ...then we run Initialize and that is frame 1, return the list
        systems |> collectAndApplyChange (fun s -> s.IsActive) (fun s -> s.Initialize ecd) ecd

    let Update ecd systems = 
        printfn "%A" (Keyboard.GetState().ToString)
        []
        //match handleKeyboardInput (Keyboard.GetState()) with
        //| false -> systems |> collectAndApplyChange (fun s -> s.IsActive && s.IsInitialized) (fun s -> s.Update ecd) ecd
        //| true -> List.empty

