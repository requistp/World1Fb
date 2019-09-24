open GameManager
open MapGenerator
open Renderer

let g = Game(MakeMap)

g.Update |> ignore

//printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID

RenderFrame g.Frame_Current
