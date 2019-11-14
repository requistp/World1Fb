module LocationTypes
open CommonGenericFunctions
open System

[<Literal>]
let MapWidth = 30


[<Literal>]
let MapHeight = 30


let IsOnMap2D x y = 
    x >= 0 && x <= MapWidth-1 && y >=0 && y <= MapHeight-1

type LocationDataInt = {
    X : int
    Y : int
    Z : int
    } with
    member me.Add (l:LocationDataInt) = { X = me.X + l.X; Y = me.Y + l.Y; Z = me.Z + l.Z }
    member me.AddOffset (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
        me.Add (LocationDataInt.Offset rangeX rangeY rangeZ allow000 doubleRandom)
    member me.IsOnMap = IsOnMap2D me.X me.Y
    member me.Subtract (l:LocationDataInt) = { X = me.X - l.X; Y = me.Y - l.Y; Z = me.Z - l.Z }

    override me.ToString() = sprintf "{X=%i, Y=%i, Z=%i}" me.X me.Y me.Z

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

let Distance2D (l1:LocationDataInt) (l2:LocationDataInt) =
    Math.Pow (Math.Pow (float (l1.X - l2.X), 2.0) + Math.Pow (float (l1.Y - l2.Y), 2.0), 0.5)

let WithinRange2D (l1:LocationDataInt) (l2:LocationDataInt) (range:int) =
    int (Math.Round(Distance2D l1 l2)) <= range

let RangeTemplate2D (range:int) =
    [| -range .. range |] 
    |> Array.Parallel.collect (fun y -> [| -range .. range |] |> Array.map (fun x -> { X=x; Y=y; Z=0 } ))
    |> Array.filter (fun l -> WithinRange2D LocationDataInt.empty l range)

//let LocationsWithinRange2D location range = 
//    range 
//    |> RangeTemplate2D 
//    |> Array.Parallel.map (fun l -> l.Add location)
//    |> Array.filter (fun l -> l.IsOnMap)

let LocationsWithinRange2D location (rangeTemplate:LocationDataInt[]) = 
    rangeTemplate 
    |> Array.Parallel.map (fun l -> l.Add location)
    |> Array.filter (fun l -> l.IsOnMap)

let MapLocations =
    [|0..MapHeight-1|] |> Array.collect (fun y -> [|0..MapWidth-1|] |> Array.map (fun x -> { X=x; Y=y; Z=0 } ))



