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

let systems = [
    FormSystem(true) :> AbstractSystem
    TerrainSystem(true) :> AbstractSystem
    ]

let mutable _frames = List.empty:Frame list
//let ecd = MakeMap { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u }
let ecd = MakeRabbits (MakeMap { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u }) 3
let f = Game.Initialize ecd systems

RenderFrame f

//let g = Game(sl, RenderFrame)

//let f = g.InitializeGame


//let r2 = g.Update
//match r2 with
//| Ok f -> printfn "r2"; RenderFrame f
//| Error e -> printfn "r2 Error:%O" e; ()

//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
