open EntityComponentManager
open GameManager
open MapGenerator
open Renderer

//let ecmap = MakeMap EntityComponentMap.New

//let ecman = EntityComponentManager ecmap

let g = Game(MakeMap EntityComponentData.New)


g.StartGame_New

g.Update |> ignore

//printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID

RenderFrame g.Frame_Current

RenderFrame g.Frame_Current