module LocationTypes
open CommonGenericFunctions
open System

[<Literal>]
let MapWidth = 40


[<Literal>]
let MapHeight = 20


type LocationDataInt = {
    X : int
    Y : int
    Z : int
    } with
    member me.Add (l:LocationDataInt) = { X = me.X + l.X; Y = me.Y + l.Y; Z = me.Z + l.Z }
    member me.AddOffset (rangeX:int) (rangeY:int) (rangeZ:int) (allow000:bool) (doubleRandom:bool) =
        me.Add (LocationDataInt.Offset rangeX rangeY rangeZ allow000 doubleRandom)
    member me.IsOnMap = if me.X >= 0 && me.X <= MapWidth-1 && me.Y >=0 && me.Y <= MapHeight-1 then true else false
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
        
let MapLocations =
    [|0..MapHeight-1|] |> Array.collect (fun y -> [|0..MapWidth-1|] |> Array.map (fun x -> { X=x; Y=y; Z=0 } ))


    //for y in [0..MapHeight-1] do
    //    for x in [0..MapWidth-1] do
    //        Arr
    //        |> Array.fold ( )
    //ComponentTypes.AsArray
    //|> Array.fold (fun (m:Map<ComponentTypes,EntityArray>) ct -> m.Add(ct,new EntityArray())) Map.empty