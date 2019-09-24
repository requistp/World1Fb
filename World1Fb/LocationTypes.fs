module LocationTypes

[<Literal>]
let MapWidth = 1000
[<Literal>]
let MapHeight = 1000

[<Struct>]
type LocationDataInt(x:int, y:int) =
    member this.X = x
    member this.Y = y

[<Struct>]
type LocationDataFloat(x:float, y:float) = 
    member this.X = x
    member this.Y = y
    with member this.Xint = int (round this.X)
         member this.Yint = int (round this.Y)
         member this.LocationAsInt = LocationDataInt(this.Xint, this.Yint)

let OnMapInt (l:LocationDataInt) = if l.X >= 0 && l.X <= MapWidth && l.Y >=0 && l.Y <= MapHeight then true else false
