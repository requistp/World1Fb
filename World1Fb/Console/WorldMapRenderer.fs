module WorldMapRenderer
open ComponentEnums
open FormComponent
open EntityManager
open LocationTypes
open System


type WorldMapRenderer() =
    let mutable _windowLocation = (0,0)
    let viewSizeX = Math.Clamp(40,10,MapWidth)
    let viewSizeY = Math.Clamp(20,10,MapHeight)
    
    member this.MoveWindow (k:ConsoleKey) =
        match k with 
        | ConsoleKey.UpArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation - 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.DownArrow -> _windowLocation <- (fst _windowLocation, Math.Clamp(snd _windowLocation + 1, 0, MapHeight-viewSizeY))
        | ConsoleKey.LeftArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation - 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | ConsoleKey.RightArrow -> _windowLocation <- (Math.Clamp(fst _windowLocation + 1, 0, MapWidth-viewSizeX), snd _windowLocation)
        | _ -> ()

    member this.Update (enm:EntityManager) = 
        //Console.Clear()
        //Console.SetBufferSize(MapWidth,MapHeight)
        //Console.SetBufferSize(Math.Max(MapWidth,Console.WindowWidth), Math.Max(MapHeight,Console.WindowHeight))
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
                    enm.GetEntitiesAtLocation { X = x; Y = y; Z = 0 } 
                    |> enm.TryGetComponentForEntities FormComponentID
                    |> Array.map (fun c -> c.ToForm)

                match fs.Length with
                | 0 -> ()
                | _ ->
                    let fd = selectForm fs
                    System.Console.SetCursorPosition(x - fst _windowLocation, y - snd _windowLocation)
                    System.Console.Write(fd.Symbol)

        



