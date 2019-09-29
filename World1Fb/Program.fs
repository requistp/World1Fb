open GameManager
open Renderer
open SystemManager
open FormSystem
open TerrainSystem

let sl = [
    FormSystem(true) :> AbstractSystem
    TerrainSystem(true) :> AbstractSystem
    ]

let g = Game(sl, RenderFrame)

let f = g.InitializeGame


//let r2 = g.Update
//match r2 with
//| Ok f -> printfn "r2"; RenderFrame f
//| Error e -> printfn "r2 Error:%O" e; ()

//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
