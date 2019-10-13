module LocationTypes
open CommonGenericFunctions

[<Literal>]
let MapWidth = 40

[<Literal>]
let MapHeight = 20

[<Struct>]
type LocationDataInt = {
    X : int
    Y : int
    } with
    member this.Add (x,y) = { X = this.X + x; Y = this.Y + y }
    member this.Add (l2:LocationDataInt) = { X = this.X + l2.X; Y = this.Y + l2.Y }
    member this.IsOnMap = if this.X >= 0 && this.X <= MapWidth-1 && this.Y >=0 && this.Y <= MapHeight-1 then true else false

