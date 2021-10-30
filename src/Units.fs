namespace Engine

open FSharp.Data.UnitSystems.SI.UnitNames


[<AutoOpen>]
module Units =
    [<Measure>]
    type s = second

    [<Measure>]
    type ms =
        static member perSecond = 1e3<ms/s>
        static member toSecond(x: float<ms>) = x / ms.perSecond

    [<Measure>]
    type px
