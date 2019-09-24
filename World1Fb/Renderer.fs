module Renderer
open Components
open GameManager
open LocationTypes

let DrawAt (c:char) (l:LocationDataInt) =
    System.Console.SetCursorPosition(l.X,l.Y)
    System.Console.Write(c)

let RenderTerrain (f:Frame) =
    let st = System.DateTime.Now

    for e in f.ECMan.EntitiesWithComponent ComponentID_Terrain do
        match f.ECMan.GetEntityComponent ComponentID_Terrain e with
        | Terrain x -> ()//DrawAt x.Symbol x.Location
        | _ -> ()
        //| Terrain x -> let DrawAtAsync = async { DrawAt x.Symbol x.Location }; 
        //               Async.Start DrawAtAsync 
    let et = System.DateTime.Now
    printfn "\nRender terrain start:%O, %i" st st.Millisecond
    printfn "\nRender terrain end  :%O, %i" et et.Millisecond

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
