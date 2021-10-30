namespace Engine

open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop


[<AutoOpen>]
module Canvas =
    /// TODO: Build an actual abstraction over the canvas drawing mechanism.
    /// Check https://github.com/davedawkins/Fable.React.DrawingCanvas.
    type Canvas = CanvasRenderingContext2D
    type CanvasRenderingContext2D with
        member this.Width = int this.canvas.width * 1<px>
        member this.Height = int this.canvas.height * 1<px>

    // copied from https://github.com/fable-compiler/Fable/blob/main/src/Fable.Core/Fable.Core.JsInterop.fs
    let inline (!^) (x:^t1) : ^t2 =
        ((^t1 or ^t2) : (static member op_ErasedCast : ^t1 -> ^t2) x)

    let rgb r g b =
        $"rgb(%d{r}, %d{g}, %d{b})"

    let rgba r g b a =
        $"rgb(%d{r}, %d{g}, %d{b}, %f{a})"
