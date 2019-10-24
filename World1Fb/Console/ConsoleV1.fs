module ConsoleV1
open System
open GameManager


type Window(wd:(string * string)) =

    member _.Content = (snd wd)
    member _.Name = (fst wd)
    
    member _.Display =
        Console.SetCursorPosition(0,0)
        Console.Write (snd wd)
        

type WindowManager(wds:(string * string)[]) =
    let mutable _windows = Map.empty<string,Window>
    let mutable _current = (fst wds.[0])
       
    let setContent (wds:(string * string)[]) =
           _windows <-
               wds
               |> Array.fold (fun ws w -> ws.Add((fst w),Window(w))) Map.empty<string,Window>

    do 
        System.Console.CursorVisible <- false
        setContent wds
        Console.Title <- _current

    member this.Count = _windows.Count

    member this.Display (s:string) =
        match _current with
        | "Game Events List" -> Console.Clear()
        | _ -> ()

        _windows.Item(_current).Display
        Console.WriteLine s

    member this.SetContent (wds:(string * string)[]) (display:bool) =
        async
            {
                setContent wds
                if display then this.Display ""
            }
    member this.SetDisplay (uniqueName:string) =
        match uniqueName <> _current && _windows.ContainsKey(uniqueName) with
        | false -> ()
        | true -> 
            _current <- uniqueName
            Console.Title <- uniqueName
            Console.Clear()
            this.Display ""
        


//[<Literal>]
//let WidthPerWindow = 100

//type windowData = string * string

//  let mutable _content = (snd wd)
//    member this.SetContent value = _content <- value

//member this.NewWindow (uniqueName:string) (content:string) =
//    match _windows.ContainsKey(uniqueName) with
//    | true -> ()
//    | false -> 
//        let w = Window(uniqueName, content)
//        _windows <- _windows.Add(uniqueName,w)            
//member this.SetContent (uniqueName:string) (content:string) (display:bool) =
//    match _windows.ContainsKey(uniqueName) with
//    | false -> ()
//    | true -> 
//        _windows.Item(uniqueName).SetContent content
//        if display then this.Display uniqueName

        //_windows <-
        //    wds
        //    |> Array.fold (fun ws w -> ws.Add((fst w),Window(w))) Map.empty<string,Window>


            (*
            
            
            type Window(uniqueName:string, content:string) = //, width:int, height:int, bufferStartX:int, bufferStartY:int) = 
                let mutable _content = content
            
                member _.Content = _content
                member _.Name = uniqueName
                //member _.Width = width
                //member _.Height = height
            
                //member _.BufferStartX = bufferStartX
                //member _.BufferStartY = bufferStartY
            
                member this.SetContent value = _content <- value
            
                member this.WriteToBuffer =
                    
            
            
            type Windows() =
                let mutable _windows = Map.empty<string,Window>
            
            
                member this.NewWindow (uniqueName:string) (content:string) (width:int) (height:int) =
                    //let bufferStartX = _windows.Count * WidthPerWindow
                    //let bufferStartY = 0
                    
                    match _windows.ContainsKey(uniqueName) with
                    | true -> Error (sprintf "Name:%s already exists" uniqueName)
                    | false -> 
                        let w = Window(uniqueName, content) //, width, height, bufferStartX, bufferStartY)
                        _windows <- _windows.Add(uniqueName,w)
                        //Console.BufferWidth <- _windows.Count * WidthPerWindow
                        Ok w
            
            
            
            *)