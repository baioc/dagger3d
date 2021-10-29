/// Elmish SPA in which we embed the game engine.
module App

open System.Runtime.CompilerServices
open FSharp.Data.UnitSystems.SI.UnitSymbols

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


[<Extension>]
type Bulma =
    /// Container that centers it children both vertically and horizontally.
    [<Extension>]
    static member centered (props: list<IReactProperty>) =
        Bulma.container [
            container.isFluid
            prop.children [
                Bulma.level [
                    prop.children [
                        Bulma.levelItem props
                    ]
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
    static member centered (children: seq<ReactElement>) =
        Bulma.centered [ prop.children children ]


// XXX: for some reason, these are missing from Fable.Browser.Dom
type CanvasRenderingContext2D with
    member this.imageSmoothingEnabled
        with [<Emit("$0.imageSmoothingEnabled")>] get() : bool = jsNative
        and [<Emit("$0.imageSmoothingEnabled = $1")>] set(value: bool) : unit = jsNative


let CanvasId = "canvas"

let getKey (e: KeyboardEvent) =
    { Key = e.key
      Code = e.code }

let relativeXY clientX clientY =
    let canvas = document.getElementById(CanvasId) :?> HTMLCanvasElement
    let rect = canvas.getBoundingClientRect()
    let x = clientX - rect.left
    let y = clientY - rect.top
    x, y

let getButton (e: MouseEvent) =
    let x, y = relativeXY e.clientX e.clientY
    match int e.button with
    | 0 -> Some { Button = LeftClick; X = x; Y = y }
    | 1 -> Some { Button = MiddleClick; X = x; Y = y }
    | 2 -> Some { Button = RightClick; X = x; Y = y }
    | _ -> None

let getMovement (e: MouseEvent) =
    let x, y = relativeXY e.clientX e.clientY
    { Dx = e.movementX; Dy = e.movementY
      X = x; Y = y }

let getTouch (e: TouchEvent) =
    { Changed =
        e.changedTouches
        |> Seq.map (fun t ->
            let x, y = relativeXY t.clientX t.clientY
            int64 t.identifier, { Id = int64 t.identifier; X = x; Y = y })
        |> Map.ofSeq }

let page game dispatch =
    // the canvas may not yet exist on the first call to this view
    let canvas = document.getElementById(CanvasId)
    if not (isNull canvas) then
        let canvas = canvas :?> HTMLCanvasElement
        let ctx = canvas.getContext_2d()
        do
            if ctx.imageSmoothingEnabled then ctx.imageSmoothingEnabled <- false
            Engine.draw ctx game

    Bulma.hero [
        hero.isFullHeight
        prop.children [
            Bulma.heroBody [
                prop.children [
                    Bulma.centered [
                        Html.canvas [
                            prop.id CanvasId
                            prop.width (int Configuration.Width)
                            prop.height (int Configuration.Height)
                            prop.tabIndex 0 // needed for focus
                            prop.style [ style.cursor "crosshair" ]
                            prop.onKeyDown (getKey >> KeyPressed >> dispatch)
                            prop.onKeyUp (getKey >> KeyReleased >> dispatch)
                            prop.onMouseDown (fun e ->
                                if isNull document.fullscreenElement && not (isNull canvas) then
                                    canvas.requestPointerLock()
                                else
                                    e |> getButton |> Option.iter (MousePressed >> dispatch)
                            )
                            prop.onMouseUp (getButton >> Option.iter (MouseReleased >> dispatch))
                            prop.onMouseMove (getMovement >> MouseMoved >> dispatch)
                            prop.onWheel (fun e -> MouseWheelRotated { Delta = e.deltaY } |> dispatch)
                            prop.onFocus (fun _ -> FocusGained |> dispatch)
                            prop.onBlur (fun _ -> FocusLost |> dispatch)
                            prop.onTouchStart (fun e ->
                                if isNull document.fullscreenElement && not (isNull canvas) then
                                    canvas.requestFullscreen()
                                else
                                    e |> getTouch |> TouchStarted |> dispatch
                            )
                            prop.onTouchEnd (getTouch >> TouchEnded >> dispatch)
                            prop.onTouchMove (getTouch >> TouchMoved >> dispatch)
                            prop.onTouchCancel (getTouch >> TouchCancelled >> dispatch)
                        ]
                    ]
                ]
                prop.style [ style.padding 0 ]
            ]
        ]
    ]

let subscribe initial =
    Cmd.ofSub <| fun dispatch ->
        let timeStep = 1.0<s> * ms.perSecond / Configuration.FPS
        let tick () = dispatch (Tick timeStep)
        window.setInterval(tick, int timeStep) |> ignore


// notice that we're using "batched" mode. this means that Elmish updates will
// trigger a `requestAnimationFrame` instead of rendering immediately and, if a
// new update arrives before the rendering, it will substitute the previous one.
// therefore, the React diffing-renderer has freshest-value semantics and can
// work at its own (browser-chosen) rate, independently of the game time step
Program.mkProgram Engine.init Engine.handle page
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription subscribe
#if DEBUG
|> Program.withDebugger // NOTE: needs to be the last Program update before run
#endif
|> Program.run
