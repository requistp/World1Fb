open CommonGenericFunctions
open LocationTypes
open Renderer
open GameManager
open FormSystem
open TerrainSystem
open Components
open TerrainComponent
open FormComponent

let MakeMap = List.collect (fun x -> [for (y:uint16) in [0us..MapHeight - 1us] -> ComponentType.Terrain({ Type=Dirt; Location={X=x; Y=y} })]) [0us..MapWidth - 1us]

let MakeRabbit rnd = 
    let rx = (uint16 (random.Next(0,MapWidthInt-1)))
    let ry = (uint16 (random.Next(0,MapHeightInt-1)))
    let form = ComponentType.Form { IsPassable=true; Name="rabbit"; Symbol='r'; Location={X=rx;Y=ry} }
    let move = ComponentType.Movement { MovesPerTurn = 2uy }
    //sight
    //health
    [
        form
        move
    ]
let MakeRabbits n = 
    match n with 
    | 0 -> List.empty:ComponentType list list
    | _ -> [1..n] |> List.map (fun i -> MakeRabbit (random.Next()))

let systems = [
    FormSystem(true) :> AbstractSystem
    TerrainSystem(true) :> AbstractSystem
    ]

let mutable _frames = List.empty:Frame list

let f = Game.Initialize systems

//RenderFrame f

//let g = Game(sl, RenderFrame)

//let f = g.InitializeGame


//let r2 = g.Update
//match r2 with
//| Ok f -> printfn "r2"; RenderFrame f
//| Error e -> printfn "r2 Error:%O" e; ()

//g.Update |> ignore

////printfn "frame=%i entities=%i maxEntityID=%i" g.Frames.Length g.Frame_Current.ECM.EntitiesExist .Count g.Frame_Current.ECM.MaxEntityID


//RenderFrame g.Frame_Current
