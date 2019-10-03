module LocationTypes
open CommonGenericFunctions

[<Literal>] 
let MapWidth = 2000us

[<Literal>]
let MapWidthInt = 2000

[<Literal>]
let MapHeight = 2000us
[<Literal>]
let MapHeightInt = 2000

type LocationDataInt = {
    X : uint16
    Y : uint16
    }

let OnMapInt (l:LocationDataInt) = if l.X >= 0us && l.X <= MapWidth && l.Y >=0us && l.Y <= MapHeight then true else false


//[<Struct>]
//type LocationDataFloat(x:float, y:float) = 
//    member this.X = x
//    member this.Y = y
//    with member this.Xint = int (round this.X)
//         member this.Yint = int (round this.Y)
//         member this.LocationAsInt = LocationDataInt(uint16 this.Xint, uint16 this.Yint)
