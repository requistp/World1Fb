module LocationTypes

[<Literal>]
let MapWidth = 2000us

[<Literal>]
let MapHeight = 2000us

//[<Struct>]
type LocationDataInt(x:uint16, y:uint16) =
    member this.X = x
    member this.Y = y

let OnMapInt (l:LocationDataInt) = if l.X >= 0us && l.X <= MapWidth && l.Y >=0us && l.Y <= MapHeight then true else false


//[<Struct>]
//type LocationDataFloat(x:float, y:float) = 
//    member this.X = x
//    member this.Y = y
//    with member this.Xint = int (round this.X)
//         member this.Yint = int (round this.Y)
//         member this.LocationAsInt = LocationDataInt(uint16 this.Xint, uint16 this.Yint)
