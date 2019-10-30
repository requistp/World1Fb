module Renderer
open CommonGenericFunctions
open AbstractComponent
open EntityManager
open GameManager
open LocationTypes
open TerrainComponent
open FormComponent


let private DrawAt (c:char) location =
    System.Console.SetCursorPosition(int location.X, int location.Y)
    System.Console.Write(c)
   

let private RenderAll (enm:EntityManager) =
    let newrender (fd:FormData) =
        DrawAt fd.Symbol fd.Location

    let fs : FormData[] = 
        enm.GetEntitiesWithComponent 1
        |> Array.map (fun eid -> enm.TryGetComponent 1 eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)
        |> Array.map (fun f -> 
            let (Form (_,fd)) = f
            fd)
    let ts =
        enm.GetEntitiesWithComponent 2
        |> Array.map (fun eid -> enm.TryGetComponent 2 eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)
        |> Array.map (fun f -> 
            let (Terrain (_,td)) = f
            td)

    //ts |> Array.iter (fun t -> render t.EntityID fs)
    //fs |> Array.filter (fun f-> not (ts |> Array.exists (fun t -> t.EntityID=f.EntityID))) |> Array.iter (fun f -> render f.EntityID fs)
    let all = fs
    all |> Array.iter (fun f -> newrender f)
    printfn "forms:%i" all.Length
    
let RenderFrame (enm:EntityManager) (round:uint32) =
    System.Console.CursorVisible <- false 

    RenderAll enm

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Round:%i\n" round)

