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
        member this.width = int this.canvas.width

        member this.height = int this.canvas.height

        member this.color
            with get() = this.strokeStyle
            and set(color: string) =
                this.strokeStyle <- !^ color
                this.fillStyle <- !^ color

        member this.fillRect(x: int, y: int, w: int, h: int) : unit =
            this.fillRect(float x, float y, float w, float h)

        member this.moveTo(x: int, y: int) =
            this.moveTo(float x + 0.5, float y)

        member this.lineTo(x: int, y: int) =
            this.lineTo(float x + 0.5, float y)

    let rgb r g b =
        $"rgb(%d{r}, %d{g}, %d{b})"

    let rgba r g b a =
        $"rgba(%d{r}, %d{g}, %d{b}, %g{a})"
