namespace Engine


[<AutoOpen>]
module private Internal =
    let saturate (x: int) : byte =
        if x < 0 || x > 255 then byte 255 else byte x

    let normalize (x: float) : float =
        if x < 0.0 || x > 1.0 then 1.0 else x

// XXX: could be `System.Drawing.Color` if Fable supported it
[<Struct>]
type Color =
    private {
        Red: byte
        Green: byte
        Blue: byte
        Alpha: float
    }

    member this.R: int = int this.Red
    member this.G: int = int this.Green
    member this.B: int = int this.Blue
    member this.A: float = this.Alpha

[<AutoOpen>]
module Color =
    let rgba (r: int) (g: int) (b: int) (a: float) : Color =
        { Red = saturate r; Green = saturate g; Blue = saturate b; Alpha = normalize a }

    let rgb (r: int) (g: int) (b: int) : Color =
        rgba r g b 1.0


type Canvas =
    abstract member Width: int<px>
    abstract member Height: int<px>
    abstract member DrawLine: color:Color -> xa:int<px> -> ya:int<px> -> xb:int<px> -> yb:int<px> -> unit
    abstract member DrawRect: color:Color -> x:int<px> -> y:int<px> -> w:int<px> -> h:int<px> -> unit
