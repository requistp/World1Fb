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

let ecd = MakeRabbits (MakeMap Entity.EmptyECD) 1

let mutable _fl = Game.Initialize ecd systems

Console.CursorVisible <- false 

let mutable k = ConsoleKey.Z

while k <> ConsoleKey.Escape do
    RenderFrame _fl.Head
    k <- Console.ReadKey(true).Key
    printfn "\n Key:%A" k
    _fl <- Game.Update _fl.Head.ECD systems


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
