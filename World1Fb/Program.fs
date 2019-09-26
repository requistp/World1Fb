open GameManager
open MapGenerator
open Renderer
//open EntityComponentManager 

let g = Game()

let f0 = g.InitializeGame (TranslateMapToEntities g.EntityManager MakeMap)

RenderFrame g.Frame_Current

//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
