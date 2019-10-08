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

