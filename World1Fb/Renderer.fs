module Renderer
open CommonGenericFunctions
open Component
open EntityManager
open GameManager
open LocationTypes


let private DrawAt (c:char) location =
    System.Console.SetCursorPosition(int location.X, int location.Y)
    System.Console.Write(c)
   

let private RenderAll (enm:EntityManager) =
    let newrender (fd:FormData) =
        DrawAt fd.Symbol fd.Location

    let fs : FormData[] = 
        enm.GetEntitiesWithComponent FormData.ID
        |> Array.map (fun eid -> enm.TryGetComponent FormData.ID eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)
        |> Array.map (fun f -> 
            let (Form d) = f
            d)
    let ts =
        enm.GetEntitiesWithComponent TerrainData.ID
        |> Array.map (fun eid -> enm.TryGetComponent TerrainData.ID eid)
        |> Array.collect (fun fo -> fo |> Option.toArray)
        |> Array.map (fun f -> 
            let (Terrain d) = f
            d)

    //ts |> Array.iter (fun t -> render t.EntityID fs)
    //fs |> Array.filter (fun f-> not (ts |> Array.exists (fun t -> t.EntityID=f.EntityID))) |> Array.iter (fun f -> render f.EntityID fs)
    let all = fs
    all |> Array.iter (fun f -> newrender f)
    
let RenderFrame (enm:EntityManager) (round:uint32) =
    System.Console.CursorVisible <- false 

    RenderAll enm

    System.Console.SetCursorPosition(MapWidth+1,MapHeight)
    System.Console.Write(sprintf "Round:%i\n" round)

