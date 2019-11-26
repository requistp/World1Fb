module rec LocationTypes
open CommonGenericFunctions
open System

[<Literal>]
let MapWidth = 20


[<Literal>]
let MapHeight = 20


type LocationDataInt = 
    {
        X : int
        Y : int
        Z : int
    } 
    static member (+) (l1:LocationDataInt,l2:LocationDataInt) = { X = l1.X + l2.X; Y = l1.Y + l2.Y; Z = l1.Z + l2.Z }
    static member (-) (l1:LocationDataInt,l2:LocationDataInt) = { X = l1.X - l2.X; Y = l1.Y - l2.Y; Z = l1.Z - l2.Z }
    static member empty = { X = 0; Y = 0; Z = 0 }
    static member Is000 l = (l = LocationDataInt.empty)
    static member Offset (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
        let getNewLocation rnd = 
            {
                X = random.Next(-rangeX,rangeX+1)
                Y = random.Next(-rangeY,rangeY+1)
                Z = random.Next(-rangeZ,rangeZ+1)
            }
        let getNewLocation_double rnd = 
            {
                X = int (Math.Round((float (random.Next(-rangeX,rangeX+1)) + float (random.Next(-rangeX,rangeX+1))) / 2.0, 0))
                Y = int (Math.Round((float (random.Next(-rangeY,rangeY+1)) + float (random.Next(-rangeY,rangeY+1))) / 2.0, 0))
                Z = int (Math.Round((float (random.Next(-rangeZ,rangeZ+1)) + float (random.Next(-rangeZ,rangeZ+1))) / 2.0, 0))
            }
        let newLocation rnd =
            if doubleRandom then getNewLocation_double rnd else getNewLocation rnd

        let mutable l = newLocation random.Next
        while (not allow000 && LocationDataInt.Is000 l) do
            l <- newLocation random.Next
        l
    override me.ToString() = sprintf "{X=%i, Y=%i, Z=%i}" me.X me.Y me.Z

let AddLocations (l1:LocationDataInt) (l2:LocationDataInt) = l1 + l2
let SubtractLocations (l1:LocationDataInt) (l2:LocationDataInt) = l1 - l2

let AddOffset (l:LocationDataInt) (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
    l + (LocationDataInt.Offset rangeX rangeY rangeZ allow000 doubleRandom)

let IsOnMap2D (l:LocationDataInt) = 
    l.X >= 0 && l.X <= MapWidth-1 && l.Y >=0 && l.Y <= MapHeight-1

let Distance2D (l1:LocationDataInt) (l2:LocationDataInt) =
    Math.Pow (Math.Pow (float (l1.X - l2.X), 2.0) + Math.Pow (float (l1.Y - l2.Y), 2.0), 0.5)

let WithinRange2D (l1:LocationDataInt) (range:int) (l2:LocationDataInt) =
    int (Math.Round(Distance2D l1 l2)) <= range

let RangeTemplate2D (range:int) =
    [| -range .. range |] 
    |> Array.Parallel.collect (fun y -> [| -range .. range |] |> Array.map (fun x -> { X=x; Y=y; Z=0 } ))
    |> Array.filter (WithinRange2D LocationDataInt.empty range)

let LocationsWithinRange2D (location:LocationDataInt) (rangeTemplate:LocationDataInt[]) = 
    rangeTemplate 
    |> Array.Parallel.map (AddLocations location)
    |> Array.filter IsOnMap2D

let MapLocations =
    [|0..MapHeight-1|] |> Array.collect (fun y -> [|0..MapWidth-1|] |> Array.map (fun x -> { X=x; Y=y; Z=0 } ))




//let LocationsWithinRange2D location range = 
//    range 
//    |> RangeTemplate2D 
//    |> Array.Parallel.map (fun l -> l.Add location)
//    |> Array.filter (fun l -> l.IsOnMap)
