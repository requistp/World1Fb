open CommonGenericFunctions
open LocationTypes
open Renderer
open GameManager
open FormSystem
open TerrainSystem
open Components
open TerrainComponent
open FormComponent
open BuildNewWorld
open System
open EntityComponentManager

let systems = 
    [
        FormSystem(true) :> AbstractSystem
        TerrainSystem(true) :> AbstractSystem
    ]

Game.Start (MakeRabbits (MakeMap Entity.EmptyECD) 1) RenderFrame systems

//let mutable abort = false

//while not abort do
//    RenderFrame f

//    while not Console.KeyAvailable do
//        System.Threading.Thread.Sleep 250
    
//    let k = Console.ReadKey(true).Key

//    match k with
//    | ConsoleKey.Escape -> abort <- true
//    | _ -> f <- Game.Update f.ECD systems


//let mutable k = Console.K Keyboard.GetState().GetPressedKeys()
//let mutable escWasPressed = false

//while not escWasPressed do
//    k <- Keyboard.GetState().GetPressedKeys()
//    match k |> Array.toList |> List.length with
//    | 0 -> ()
//    | _ -> escWasPressed <- true
//   // ()


//while not Console.KeyAvailable do
//    let k = Console.Read()
//    ()
//let g = Game(sl, RenderFrame)

//let f = g.InitializeGame


//let r2 = g.Update
//match r2 with
//| Ok f -> printfn "r2"; RenderFrame f
//| Error e -> printfn "r2 Error:%O" e; ()

//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
