module LocationTypes
open CommonGenericFunctions

[<Literal>]
let MapWidth = 40


[<Literal>]
let MapHeight = 20


type LocationDataInt = {
    X : int
    Y : int
    Z : int
    } with
    member this.Add (l:LocationDataInt) = { X = this.X + l.X; Y = this.Y + l.Y; Z = this.Z + l.Z }
    member this.IsOnMap = if this.X >= 0 && this.X <= MapWidth-1 && this.Y >=0 && this.Y <= MapHeight-1 then true else false

