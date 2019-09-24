module Renderer
open Components
open GameManager
open LocationTypes

let DrawAt (c:char) (l:LocationDataInt) =
    System.Console.SetCursorPosition(l.X,l.Y)
    System.Console.Write(c)

let RenderTerrain (f:Frame) =
    let st = System.DateTime.Now
    
    for e in f.ECM.EntitiesWithComponent ComponentID_Terrain do
        match f.ECM.GetEntityComponent ComponentID_Terrain e with
        | Terrain x -> ()//DrawAt x.Symbol x.Location
        //| Terrain x -> let DrawAtAsync = async { DrawAt x.Symbol x.Location }; 
        //               Async.Start DrawAtAsync 
        | _ -> ()
        //old... let DrawAtAsync = async { DrawAt tc.Symbol tc.Location.X tc.Location.Y }
        //old... Async.Start DrawAtAsync 
        //DrawAt t.Symbol t.Location.X t.Location.Y
    //    ()
    let et = System.DateTime.Now
    printfn "\n%O, %i" st st.Millisecond
    printfn "\n%O, %i" et et.Millisecond

//let RenderForms (f:Frame) =
//    for e in f.ECM.EntityIDsWithComponent (typeof<FormComponent>) do
//        let ac = f.ECM.GetComponent (typeof<FormComponent>) e
//        let fc = (ac :?> FormComponent)
//        DrawAt fc.Symbol fc.Location.Xint fc.Location.Yint

let RenderFrame (f:Frame) =
    RenderTerrain f
    //RenderForms f

let RenderFrames (fl:Frame list) =
    fl |> List.iter RenderFrame
