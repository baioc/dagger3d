/// Elmish SPA in which we embed the game.
module App

open System.Runtime.CompilerServices

open Browser
open Browser.Types
open Fable.Core
open Fable.React
open Feliz
open Feliz.Bulma
open Elmish
open Elmish.React
#if DEBUG
open Elmish.Debug
open Elmish.HMR // NOTE: needs to be the last Elmish.* import
#endif

open Engine
open Engine.Input
open Game


[<Extension>]
type Bulma =
    /// Container that centers it children both vertically and horizontally.
    [<Extension>]
    static member centered(props: list<IReactProperty>) =
        Bulma.container [
            container.isFluid
            prop.children [
                Bulma.level [
                    prop.children [ Bulma.levelItem props ]
                    prop.style [
                        style.padding 0
                        style.margin 0
                    ]
                ]
            ]
            prop.style [
                style.padding 0
                style.margin 0
            ]
        ]

    [<Extension>]
    static member centered(children: seq<ReactElement>) =
        Bulma.centered [ prop.children children ]


// XXX: for some reason, these are missing from Fable.Browser.Dom
type CanvasRenderingContext2D with
    member this.imageSmoothingEnabled
        with [<Fable.Core.Emit("$0.imageSmoothingEnabled")>] get() : bool = jsNative
        and [<Fable.Core.Emit("$0.imageSmoothingEnabled = $1")>] set(value: bool) : unit = jsNative


module Program =
    /// XXX: `Program.withReactBatched` passes a hard-coded reference equality
    /// comparison to its "lazy" view. We want structural (and overridable)
    /// comparison, thus the copy-paste. https://elmish.github.io/react/react.html
    let withReact' placeholderId program =
        let mutable lastRequest = None
        let setState model dispatch =
            match lastRequest with
            | Some r -> window.cancelAnimationFrame r
            | _ -> ()

            lastRequest <- Some (window.requestAnimationFrame (fun _ ->
                ReactDom.render(
                    lazyView2With (=) (Program.view program) model dispatch, // <- changed
                    document.getElementById placeholderId
                )))

        program
        |> Program.withSetState setState


[<Literal>]
let CanvasId = "canvas"

let relativeXY clientX clientY =
    let canvas = document.getElementById (CanvasId) :?> HTMLCanvasElement
    let rect = canvas.getBoundingClientRect ()
    let x = clientX - rect.left
    let y = clientY - rect.top
    x * 1.0<px>, y * 1.0<px>

let getKey (e: Browser.Types.KeyboardEvent) =
    { Key = e.key; Code = e.code }

let getButton (e: Browser.Types.MouseEvent) =
    let x, y = relativeXY e.clientX e.clientY
    match int e.button with
    | 0 -> Some { Button = LeftClick; X = x; Y = y }
    | 1 -> Some { Button = MiddleClick; X = x; Y = y }
    | 2 -> Some { Button = RightClick; X = x; Y = y }
    | _ -> None

let getMovement (e: Browser.Types.MouseEvent) =
    let x, y = relativeXY e.clientX e.clientY
    { Dx = e.movementX * 1.0<px>; Dy = e.movementY * 1.0<px>
      X = x; Y = y }

let getWheel (e: Browser.Types.WheelEvent) =
    { Delta = e.deltaY }

let getTouch (e: Browser.Types.TouchEvent) =
    { Changed =
        e.changedTouches
        |> Seq.map (fun t ->
            let x, y = relativeXY t.clientX t.clientY
            int64 t.identifier, { Id = int64 t.identifier; X = x; Y = y })
        |> Map.ofSeq }

// wrappers
let Input e = e |> GameEvent.Input |> Engine.Internal.EngineEvent.GameEvent
let KeyPressed e = e |> KeyPressed |> KeyboardEvent |> Input
let KeyReleased e = e |> KeyReleased |> KeyboardEvent |> Input
let MousePressed e = e |> MousePressed |> MouseEvent |> Input
let MouseReleased e = e |> MouseReleased |> MouseEvent |> Input
let MouseMoved e = e |> MouseMoved |> MouseEvent |> Input
let MouseWheelRotated e = e |> MouseWheelRotated |> MouseEvent |> Input
let TouchPressed e = e |> TouchPressed |> TouchEvent |> Input
let TouchReleased e = e |> TouchReleased |> TouchEvent |> Input
let TouchMoved e = e |> TouchMoved |> TouchEvent |> Input

let page (engine: Engine.Internal.Engine<_,_>) dispatch =
    // the canvas may not yet exist on the first call to this view
    let canvas = document.getElementById(CanvasId)
    if not (isNull canvas) then
        let canvas = canvas :?> HTMLCanvasElement
        let ctx = canvas.getContext_2d()
        do
            if ctx.imageSmoothingEnabled then ctx.imageSmoothingEnabled <- false
            ctx.clearRect(0.0, 0.0, float ctx.Width, float ctx.Height)
            ctx.save()
            engine.Game.Draw engine.State ctx
            ctx.restore()

    Bulma.hero [
        hero.isFullHeight
        color.hasBackgroundDark
        prop.children [
            Bulma.heroBody [
                prop.children [
                    Bulma.centered [
                        Html.canvas [
                            prop.id CanvasId
                            prop.width (int Config.Width)
                            prop.height (int Config.Height)
                            prop.tabIndex 0 // needed for focus
                            prop.style [ style.cursor "crosshair" ]
                            color.hasBackgroundBlack
                            prop.onKeyDown (getKey >> KeyPressed >> dispatch)
                            prop.onKeyUp (getKey >> KeyReleased >> dispatch)
                            prop.onMouseDown (fun e ->
                                if isNull document.pointerLockElement && not (isNull canvas) then
                                    canvas.requestPointerLock()
                                else
                                    e |> getButton |> Option.iter (MousePressed >> dispatch))
                            prop.onMouseUp (getButton >> Option.iter (MouseReleased >> dispatch))
                            prop.onMouseMove (getMovement >> MouseMoved >> dispatch)
                            prop.onWheel (getWheel >> MouseWheelRotated >> dispatch)
                            prop.onTouchStart (fun e ->
                                if isNull document.fullscreenElement && not (isNull canvas) then
                                    canvas.requestFullscreen()
                                else
                                    e |> getTouch |> TouchPressed |> dispatch)
                            prop.onTouchEnd (fun e ->
                                    e.preventDefault() // NOTE: avoids confusion between touch and mouse
                                    e |> getTouch |> TouchReleased |> dispatch)
                            prop.onTouchCancel (getTouch >> TouchReleased >> dispatch)
                            prop.onTouchMove (getTouch >> TouchMoved >> dispatch)
                            prop.onFocus (fun _ -> Engine.Internal.WindowFocusGained |> dispatch)
                            prop.onBlur (fun _ -> Engine.Internal.WindowFocusLost |> dispatch)
                        ]
                    ]
                ]
                prop.style [ style.padding 0 ]
            ]
        ]
    ]

/// Configures global events. Everything else is hooked to the canvas' props.
let subscribe initial =
    Cmd.ofSub <| fun dispatch ->
        // NOTE: interval is set to an integer number of milliseconds, so since
        // we're rounding it down, actual FPS may be higher than the target FPS
        let timeStep = int <| (1.0<s> * ms.perSecond) / Config.FPS
        let tick () = Engine.Internal.Tick (float timeStep * 1.0<ms>) |> dispatch
        window.setInterval(tick, timeStep) |> ignore

// notice that we're using "batched" mode. this means that Elmish updates will
// trigger a `requestAnimationFrame` instead of rendering immediately and, if a
// new update arrives before the rendering, it will substitute the previous one.
// therefore, the React diffing-renderer has freshest-value semantics and can
// work at its own (browser-chosen) rate, independently of the game time step
Program.mkProgram Engine.Internal.load Engine.Internal.handleEvent page
|> Program.withReact' "elmish-app"
|> Program.withSubscription subscribe
#if DEBUG
|> Program.withDebugger // NOTE: needs to be the last Program update before run
#endif
|> Program.runWith Game.game
