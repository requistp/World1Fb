module LocationTypes
open CommonGenericFunctions

[<Literal>]
let MapWidth = 40

[<Literal>]
let MapHeight = 20

type LocationDataInt = {
    X : int
    Y : int
    } with
    member this.Add x y = { X = this.X + x; Y = this.Y + y }

let OnMapInt (l:LocationDataInt) = if l.X >= 0 && l.X <= MapWidth && l.Y >=0 && l.Y <= MapHeight then true else false


//[<Struct>]
//type LocationDataFloat(x:float, y:float) = 
//    member this.X = x
//    member this.Y = y
//    with member this.Xint = int (round this.X)
//         member this.Yint = int (round this.Y)
//         member this.LocationAsInt = LocationDataInt(uint16 this.Xint, uint16 this.Yint)
