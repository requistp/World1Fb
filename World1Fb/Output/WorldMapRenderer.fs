module WorldMapRenderer
open ComponentEnums
open FormComponent
open EntityManager
open LocationTypes
open System
//open ColoredConsole


type WorldMapRenderer() =
    let mutable _windowLocation = (0,0)
    let viewSizeX = Math.Clamp(40,10,MapWidth)
    let viewSizeY = Math.Clamp(20,10,MapHeight)
    
    member me.MoveWindow (k:ConsoleKey) =
        match k with 
        | ConsoleKey.UpArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation - 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.DownArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation + 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.LeftArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation - 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | ConsoleKey.RightArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation + 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | _ -> ()

    member me.Update (enm:EntityManager) (entityID:uint32) = 
        Console.CursorVisible <- false
        Console.Title <- "World Map"
        
        let rangeY = 
            [|(snd _windowLocation)..(snd _windowLocation + viewSizeY - 1)|]

        let rangeX = 
            [|(fst _windowLocation)..(fst _windowLocation + viewSizeX - 1)|]

        for y in rangeY do
            for x in rangeX do
                let selectForm (fds:FormComponent[]) = 
                    match fds.Length with
                    | 1 -> fds.[0]
                    | _ ->
                        match fds |> Array.tryFind (fun c -> (enm.TryGetComponent ControllerComponentID c.EntityID).IsSome) with
                        | Some f -> f
                        | None ->
                            (fds |> Array.sortBy (fun c -> (enm.TryGetComponent TerrainComponentID c.EntityID).IsSome)).[0]
                let fs = 
                    { X = x; Y = y; Z = 0 }
                    |> enm.GetEntitiesAtLocationWithComponent None FormComponentID
                    |> Array.map (fun c -> c.ToForm)

                match fs.Length with
                | 0 -> ()
                | _ ->
                    let fd = selectForm fs
                    System.Console.SetCursorPosition(x - fst _windowLocation, y - snd _windowLocation)
                    System.Console.Write(fd.Symbol)

        
    member me.UpdateEntity (enm:EntityManager) (entityID:uint32) = 
        Console.CursorVisible <- false
        Console.Title <- "Entity Viewer"
        Console.Clear()

        let centerX = 30
        let centerY = 10

        let v = (entityID |> enm.GetComponent VisionComponentID).ToVision
        let vf = (entityID |> enm.GetComponent FormComponentID).ToForm

        let addX = centerX - vf.Location.X
        let addY = centerY - vf.Location.Y

        v.ViewedMap
        |> Map.iter (fun location round -> 
            let drawX = location.X + addX
            let drawY = location.Y + addY
            match drawX >= 0 && drawY >= 0 with
            | false -> ()
            | true -> 
                let drawCall = 
                    match v.ViewableMap |> Array.contains location with
                    | false -> ColoredConsole.Console.DrawDarkGray
                    | true -> ColoredConsole.Console.DrawWhite
                let forms = 
                    let es = History.GetEntities enm (Some round) 
                    location
                    |> History.GetEntitiesAtLocation enm (Some round)
                    |> Array.Parallel.map (fun e -> (e|>History.GetComponent FormComponentID es).ToForm)
                System.Console.SetCursorPosition(drawX,drawY)
                drawCall forms.[0].Symbol
            )



