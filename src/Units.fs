namespace Engine

open System
open FSharp.Data.UnitSystems.SI.UnitNames


[<AutoOpen>]
module Units =
    [<Measure>]
    type s = second

    [<Measure>]
    type ms =
        static member perSecond = 1e3<ms/s>
        static member toSecond(x: float<ms>) : float<s> = x / ms.perSecond

    [<Measure>]
    type rad

    [<Measure>]
    type deg =
        static member perRad = 180.0<deg/rad> / Math.PI
        static member toRad(x: float<deg>) : float<rad> = x / deg.perRad

    [<Measure>]
    type px
