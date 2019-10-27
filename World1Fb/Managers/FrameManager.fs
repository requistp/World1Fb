module FrameManager
open AbstractComponent
open EventManager
open EventTypes
open System

type Frame = 
    {
        Number : uint32
        //Entities : EntityComponentDictionary //Map<uint32,AbstractComponent[]>
        MaxEntityID : uint32
    } with 
    static member empty = 
        { 
            Number = 0u
            //Entities = EntityComponentDictionary()
            MaxEntityID = 0u
        }

type GEListType = 
    | All
    | AllExceptFirst
    | Current
    | Last10Frames
    | Last10FramesExcludingFirst

type FrameManager() =
    let mutable _frames = [| Frame.empty |]
    
    member this.AddFrame (*(entities:EntityComponentDictionary)*) (maxEntityID:uint32) = 
        _frames <- 
            [| 
                { 
                    Number = (uint32 _frames.Length)
                    //Entities = entities
                    MaxEntityID = maxEntityID
                } 
            |]
            |> Array.append _frames
    
    member this.Round = Math.Clamp(_frames.[_frames.Length-1].Number, UInt32.MinValue, UInt32.MaxValue)
    member this.Frames = _frames

