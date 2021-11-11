namespace Engine

open LanguagePrimitives
open System.Runtime.CompilerServices

open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Feliz
open Feliz.Bulma
open Elmish
open Elmish.React
#if DEBUG
open Elmish.Debug
open Elmish.HMR // NOTE: needs to be the last Elmish.* import
#endif


[<Extension>]
type Bulma =
    /// Container that centers it children both vertically and horizontally.
    [<Extension>]
    static member centered(props: list<IReactProperty>) : ReactElement =
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
    static member centered(children: seq<ReactElement>) : ReactElement =
        Bulma.centered [ prop.children children ]


module Program =
    /// XXX: `Program.withReactBatched` passes a hard-coded reference equality
    /// comparison to its "lazy" view. We want structural (and overridable)
    /// comparison, thus the copy-paste. https://elmish.github.io/react/react.html
    let withReact' placeholderId program =
        // NOTE: we actually change equality slightly, forcing the first render to
        // happen such that our HTML elements will appear on the DOM at least once
        let mutable hasRenderedOnce = false
        let equals a b =
            if not hasRenderedOnce then
                do hasRenderedOnce <- true
                false
            else
                a = b

        let mutable lastRequest = None
        let setState model dispatch =
            match lastRequest with
            | Some r -> window.cancelAnimationFrame r
            | _ -> ()

            lastRequest <- Some (window.requestAnimationFrame (fun _ ->
                ReactDom.render(
                    lazyView2With equals (Program.view program) model dispatch, // <- changed
                    document.getElementById placeholderId
                )))

        program
        |> Program.withSetState setState


[<RequireQualifiedAccess>]
module Internal =

    // XXX: for some reason, these are missing from Fable.Browser.Dom
    type CanvasRenderingContext2D with
        member this.imageSmoothingEnabled
            with [<Fable.Core.Emit("$0.imageSmoothingEnabled")>] get() : bool = jsNative
            and [<Fable.Core.Emit("$0.imageSmoothingEnabled = $1")>] set(value: bool) : unit = jsNative


    [<Literal>]
    let CANVAS_ID = "canvas"

    let relativeXY clientX clientY =
        let canvas = document.getElementById (CANVAS_ID)
        if isNull canvas then
            clientX * 1.0<px>, clientY * 1.0<px>
        else
            let canvas = canvas :?> HTMLCanvasElement
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
        { Dx = e.movementX * 1.0<px>; Dy = e.movementY * 1.0<px>; X = x; Y = y }

    let getWheel (e: Browser.Types.WheelEvent) =
        { Delta = e.deltaY }

    let getTouch (e: Browser.Types.TouchEvent) =
        { Changed =
            e.changedTouches
            |> Seq.map (fun t ->
                let x, y = relativeXY t.clientX t.clientY
                int64 t.identifier, { Id = int64 t.identifier; X = x; Y = y })
            |> Map.ofSeq }

    // constructor wrappers
    let Input e = e |> GameEvent.Input |> Internal.EngineEvent.GameEvent
    let KeyPressed e = e |> KeyPressed |> Input
    let KeyReleased e = e |> KeyReleased |> Input
    let MousePressed e = e |> MousePressed |> Input
    let MouseReleased e = e |> MouseReleased |> Input
    let MouseMoved e = e |> MouseMoved |> Input
    let MouseWheelRotated e = e |> MouseWheelRotated |> Input
    let TouchPressed e = e |> TouchPressed |> Input
    let TouchReleased e = e |> TouchReleased |> Input
    let TouchMoved e = e |> TouchMoved |> Input

    /// React SPA in which we embed the game.
    let page (config: Config) (engine: Internal.Engine<'model, 'msg>) dispatch =
        // the canvas may not yet exist on the first call to this view
        let canvas = document.getElementById(CANVAS_ID)
        if not (isNull canvas) then
            let canvas = canvas :?> HTMLCanvasElement
            let ctx = canvas.getContext_2d()
            let canvas =
                let colorString (c: Color) = $"rgba(%d{c.R}, %d{c.G}, %d{c.B}, %g{c.A})"
                { new Canvas with
                    override __.Width = config.Width
                    override __.Height = config.Height
                    override __.DrawLine color xa ya xb yb =
                        ctx.strokeStyle <- !^ (colorString color)
                        ctx.beginPath()
                        ctx.moveTo(float xa + 0.5, float ya)
                        ctx.lineTo(float xb + 0.5, float yb)
                        ctx.stroke()
                    override __.DrawRect color x y w h =
                        ctx.fillStyle <- !^ (colorString color)
                        ctx.fillRect(float x, float y, float w, float h) }
            do
                ctx.save()
                ctx.imageSmoothingEnabled <- false
                ctx.lineWidth <- 1.0
                ctx.lineCap <- "butt"
                ctx.globalAlpha <- 1.0
                ctx.clearRect(0.0, 0.0, float config.Width, float config.Height)
                engine.Game.Draw engine.GameState canvas
                ctx.restore()

        Bulma.hero [
            hero.isFullHeight
            color.hasBackgroundDark
            prop.children [
                Bulma.heroBody [
                    prop.children [
                        Bulma.centered [
                            Html.canvas [
                                prop.id CANVAS_ID
                                prop.width (int config.Width)
                                prop.height (int config.Height)
                                prop.tabIndex 0 // needed for focus
                                color.hasBackgroundBlack
                                prop.onKeyDown (fun e ->
                                    e.preventDefault() // NOTE: prevents tabbing out of focus
                                    e |> getKey |> KeyPressed |> dispatch)
                                prop.onKeyUp (getKey >> KeyReleased >> dispatch)
                                prop.onMouseDown (fun e ->
                                    if not (isNull canvas) && document.pointerLockElement <> (canvas :> Element) then
                                        canvas.requestPointerLock()
                                    e |> getButton |> Option.iter (MousePressed >> dispatch))
                                prop.onMouseUp (getButton >> Option.iter (MouseReleased >> dispatch))
                                prop.onMouseMove (getMovement >> MouseMoved >> dispatch)
                                prop.onWheel (getWheel >> MouseWheelRotated >> dispatch)
                                prop.onTouchStart (fun e ->
                                    if document.fullscreenEnabled && not (isNull canvas) && document.fullscreenElement <> (canvas :> Element) then
                                        canvas.requestFullscreen()
                                    e |> getTouch |> TouchPressed |> dispatch)
                                prop.onTouchEnd (fun e ->
                                    e.preventDefault() // NOTE: avoids confusion between touch and mouse
                                    e |> getTouch |> TouchReleased |> dispatch)
                                prop.onTouchCancel (getTouch >> TouchReleased >> dispatch)
                                prop.onTouchMove (getTouch >> TouchMoved >> dispatch)
                            ]
                        ]
                    ]
                    prop.style [ style.padding 0 ]
                ]
            ]
        ]

    /// Configures global events. Everything else is hooked to the canvas' props.
    let subscribe config _ =
        Cmd.ofSub <| fun dispatch ->
            // NOTE: interval is set to an integer number of milliseconds: since
            // we're rounding it down, actual FPS may be higher than the target
            let timeStep = 1.0<s> * ms.perSecond / config.FPS
            let timeStep = FloatWithMeasure <| floor (float timeStep)
            let tick () = Internal.Tick timeStep |> dispatch
            window.setInterval(tick, int timeStep) |> ignore


[<RequireQualifiedAccess>]
module Game =
    /// Starts the game loop. XXX: `inline` needed by Fable
    let inline run (game: Game.Instance<'model, 'msg>) =
        // notice that we're using "batched" mode. this means that Elmish updates will
        // trigger a `requestAnimationFrame` instead of rendering immediately and, if a
        // new update arrives before the rendering, it will substitute the previous one.
        // therefore, the React diffing-renderer has freshest-value semantics and can
        // work at its own (browser-chosen) rate, independently of the game time step
        Program.mkProgram Internal.load Internal.handleEvent (Internal.page game.Config)
        |> Program.withReact' "elmish-app"
        |> Program.withSubscription (Internal.subscribe game.Config)
        #if DEBUG
        |> Program.withDebugger // NOTE: needs to be the last Program update before run
        #endif
        |> Program.runWith game.Game
