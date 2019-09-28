open GameManager
open MapGenerator
open Renderer
open System_Abstract
open SystemManager
open FormSystem
open TerrainSystem

let sl = [
    //FormSystem(true) :> AbstractSystem
    TerrainSystem(true) :> AbstractSystem
    ]

let g = Game()

let r = g.InitializeGame sl
match r with
| Ok f -> RenderFrame f
| _ -> ()


//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
