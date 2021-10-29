module Engine

open FSharp.Data.UnitSystems.SI.UnitSymbols

open Elmish


[<Measure>]
type ms =
    static member perSecond = 1e3<ms/s>
    static member toSecond (x: float<ms>) = x / ms.perSecond

[<Measure>]
type px


type KeyboardInput =
    { Code: string
      Key: string }

type MouseButton = LeftClick | MiddleClick | RightClick

type MouseInput =
    { Button: MouseButton
      X: float
      Y: float }

type MouseMovement =
    { Dx: float
      Dy: float
      X: float
      Y: float }

type MouseWheelRotation =
    { Delta: float }

type TouchId = int64

type ScreenTouch =
    { Id: TouchId
      X: float
      Y: float }

type TouchInput =
    { Changed: Map<TouchId, ScreenTouch>  }

type Event =
    | Tick of dt:float<ms>
    | KeyPressed of key:KeyboardInput
    | KeyReleased of key:KeyboardInput
    | MousePressed of button:MouseInput
    | MouseReleased of button:MouseInput
    | MouseMoved of MouseMovement
    | MouseWheelRotated of MouseWheelRotation
    | TouchStarted of TouchInput
    | TouchEnded of TouchInput
    | TouchMoved of TouchInput
    | TouchCancelled of TouchInput
    | FocusGained
    | FocusLost


[<RequireQualifiedAccess>]
module Configuration =
    let Width = 640<px>
    let Height = 360<px>
    let FPS = 30.0

type State =
    { PressedKeys: Set<string>
      PressedButtons: Set<MouseButton>
      ActiveTouches: Map<TouchId, ScreenTouch>
      MousePosition: float * float }

let init () =
    { PressedKeys = Set.empty
      PressedButtons = Set.empty
      ActiveTouches = Map.empty
      MousePosition = 0.0, 0.0 },
    Cmd.none


let handle event state =
    let state =
        match event with
        | Tick _ -> state

        | KeyPressed key ->
            { state with
                  PressedKeys = Set.add key.Code state.PressedKeys }

        | KeyReleased key ->
            { state with
                  PressedKeys = Set.remove key.Code state.PressedKeys }

        | MousePressed btn ->
            { state with
                  PressedButtons = Set.add btn.Button state.PressedButtons }

        | MouseReleased btn ->
            { state with
                  PressedButtons = Set.remove btn.Button state.PressedButtons }

        | MouseMoved m ->
            { state with MousePosition = m.X, m.Y }

        | MouseWheelRotated _ -> state

        | TouchStarted tch ->
            let started = tch.Changed
            { state with
                  ActiveTouches =
                    Map.toSeq state.ActiveTouches
                    |> Seq.append (Map.toSeq started)
                    |> Map.ofSeq }

        | TouchEnded tch ->
            let ended = tch.Changed
            { state with
                  ActiveTouches =
                    state.ActiveTouches
                    |> Map.filter (fun id touch -> not (Map.containsKey id ended)) }

        | TouchMoved _ -> state

        | TouchCancelled tch ->
            let cancelled = tch.Changed
            { state with
                  ActiveTouches =
                    state.ActiveTouches
                    |> Map.filter (fun id touch -> not (Map.containsKey id cancelled)) }

        | FocusGained _ -> state

        | FocusLost _ -> state

    state, Cmd.none


open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop

[<AutoOpen>]
module Canvas =
    let rgb r g b =
        $"rgb(%d{r}, %d{g}, %d{b})"

    let rgba r g b a =
        $"rgb(%d{r}, %d{g}, %d{b}, %f{a})"

let draw (ctx: CanvasRenderingContext2D) model =
    ctx.clearRect(0.0, 0.0, float Configuration.Width, float Configuration.Height)
    ctx.fillStyle <- !^ (rgba 255 0 255 0.5)
    ctx.fillRect(0.0, 0.0, float Configuration.Width, float Configuration.Height)
