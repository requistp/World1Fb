module FrameManager
open AbstractComponent
open GameEvents


type Frame = 
    {
        Number : uint32
        Entities : Map<uint32,AbstractComponent[]>
        MaxEntityID : uint32 
        GEResults : (AbstractGameEvent * Result<string option,string>)[]
        SetResult : Result<string option,string>
    } with 
    static member empty = 
        { 
            Number = 0u
            Entities = Map.empty
            MaxEntityID = 0u
            GEResults = Array.empty
            SetResult = Ok None
        }


type FrameManager() =
    let mutable _frames = [| Frame.empty |]

    member this.AddFrame (entities:Map<uint32,AbstractComponent[]>) (maxEntityID:uint32) (geResults:(AbstractGameEvent * Result<string option,string>)[]) (setResult:Result<string option,string>) =
        let f = 
            { 
                Number = (uint32 _frames.Length)
                Entities = entities
                MaxEntityID = maxEntityID
                GEResults = geResults
                SetResult = setResult
            } 
        _frames <- Array.append _frames [|f|]
        f
    
    member this.Count = _frames.Length

