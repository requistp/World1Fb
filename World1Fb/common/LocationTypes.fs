﻿module LocationTypes
open CommonGenericFunctions
open System

[<Literal>]
let MapWidth = 100uy


[<Literal>]
let MapHeight = 100uy


type LocationDataInt = {
    X : byte
    Y : byte
    Z : byte
    } with
    member this.Add (l:LocationDataInt) = { X = this.X + l.X; Y = this.Y + l.Y; Z = this.Z + l.Z }
    member this.AddOffset (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
        this.Add (LocationDataInt.Offset rangeX rangeY rangeZ allow000 doubleRandom)
    member this.IsOnMap = if this.X >= 0uy && this.X <= MapWidth-1uy && this.Y >=0uy && this.Y <= MapHeight-1uy then true else false
    member this.Print = sprintf "{X=%i, Y=%i, Z=%i}" this.X this.Y this.Z

    static member empty = { X = 0uy; Y = 0uy; Z = 0uy }
    static member Is000 l = (l = LocationDataInt.empty)
    static member Offset (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
        let getNewLocation rnd = 
            {
                X = byte (random.Next(-rangeX,rangeX+1))
                Y = byte (random.Next(-rangeY,rangeY+1))
                Z = byte (random.Next(-rangeZ,rangeZ+1))
            }
        let getNewLocation_double rnd = 
            {
                X = byte (Math.Round((float (random.Next(-rangeX,rangeX+1)) + float (random.Next(-rangeX,rangeX+1))) / 2.0, 0))
                Y = byte (Math.Round((float (random.Next(-rangeY,rangeY+1)) + float (random.Next(-rangeY,rangeY+1))) / 2.0, 0))
                Z = byte (Math.Round((float (random.Next(-rangeZ,rangeZ+1)) + float (random.Next(-rangeZ,rangeZ+1))) / 2.0, 0))
            }
        let newLocation rnd =
            if doubleRandom then getNewLocation_double rnd else getNewLocation rnd

        let mutable l = newLocation random.Next
        while (not allow000 && LocationDataInt.Is000 l) do
            l <- newLocation random.Next
        l
        
