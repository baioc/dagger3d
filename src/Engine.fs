module Engine

open Elmish

open Engine
open Engine.Input


/// Game engine configuration and compile-time constants.
[<RequireQualifiedAccess>]
module Config =
    let Width = 640<px>
    let Height = 360<px>

    let FPS =
#if DEBUG
        10.0
#else
        30.0
#endif


/// Elmish wrapper mixing engine and custom messages. See `Cmd` extension.
type GameEvent<'custom> =
    | Input of InputEvent
    | Custom of 'custom

[<RequireQualifiedAccess>]
module Cmd =
    let ofCustom msg = msg |> Custom |> Cmd.ofMsg
    let ofInput msg = msg |> Input |> Cmd.ofMsg

/// Wraps the game state with some extra runtime information.
type Runtime<'game> =
    { State: 'game
      Input: Input }

[<Struct>]
type Game<'model, 'msg> =
    { Init: unit -> 'model * Cmd<GameEvent<'msg>>
      Update: float<ms> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>
      Handler: GameEvent<'msg> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>
      Draw: 'model -> Canvas -> unit }


/// For library authors only.
[<RequireQualifiedAccess>]
module Internal =
    // we consider engine states to be equal when the game state is equal
    [<CustomEquality>] [<NoComparison>]
    type Engine<'state, 'event when 'state: equality> =
        { PressedKeys: Set<string> // stores `Code`s, not `Key`s (despite the name)
          PressedButtons: Set<MouseButton>
          PressedTouches: Map<TouchId, ScreenTouch>
          Game: Game<'state, 'event>
          State: 'state }

        override this.Equals(other) =
            match other with
            | :? Engine<'state, 'event> as other -> this.State = other.State
            | _ -> false

        override this.GetHashCode() =
            this.State.GetHashCode()

    type EngineEvent<'custom> =
        | Tick of dt:float<ms>
        | GameEvent of GameEvent<'custom>
        | WindowFocusGained
        | WindowFocusLost

    let load game =
        let state, cmd = game.Init()
        { PressedKeys = Set.empty
          PressedButtons = Set.empty
          PressedTouches = Map.empty
          Game = game
          State = state },
        Cmd.map GameEvent cmd

    let inline private makeRuntime engine =
        let isKeyPressed key = Set.contains key engine.PressedKeys
        let isMouseButtonPressed btn = Set.contains btn engine.PressedButtons
        let isTouchPressed tch = Map.containsKey tch engine.PressedTouches
        { State = engine.State
          Input =
            { IsKeyPressed = isKeyPressed
              IsMouseButtonPressed = isMouseButtonPressed
              IsTouchPressed = isTouchPressed } }

    let handleEvent event engine =
        match event with
        | Tick dt ->
            let state, cmd = engine.Game.Update dt (makeRuntime engine)
            { engine with State = state }, Cmd.map GameEvent cmd

        | GameEvent event ->
            let engine =
                match event with
                | Input (KeyboardEvent (KeyPressed key)) ->
                    { engine with
                        PressedKeys = Set.add key.Code engine.PressedKeys }
                | Input (KeyboardEvent (KeyReleased key)) ->
                    { engine with
                        PressedKeys = Set.remove key.Code engine.PressedKeys }
                | Input (MouseEvent (MousePressed btn)) ->
                    { engine with
                        PressedButtons = Set.add btn.Button engine.PressedButtons }
                | Input (MouseEvent (MouseReleased btn)) ->
                    { engine with
                        PressedButtons = Set.remove btn.Button engine.PressedButtons }
                | Input (TouchEvent (TouchPressed tch)) ->
                    let pressed = tch.Changed
                    { engine with
                        PressedTouches =
                            Map.toSeq engine.PressedTouches
                            |> Seq.append (Map.toSeq pressed)
                            |> Map.ofSeq }
                | Input (TouchEvent (TouchReleased tch)) ->
                    let released = tch.Changed
                    { engine with
                        PressedTouches =
                            engine.PressedTouches
                            |> Map.filter (fun id _ -> not (Map.containsKey id released)) }
                | Input (MouseEvent (MouseMoved _))
                | Input (MouseEvent (MouseWheelRotated _))
                | Input (TouchEvent (TouchMoved _))
                | Custom _ -> engine

            let state, cmd = engine.Game.Handler event (makeRuntime engine)
            { engine with State = state }, Cmd.map GameEvent cmd

        | WindowFocusLost ->
            { engine with
                PressedKeys = Set.empty
                PressedButtons = Set.empty
                PressedTouches = Map.empty },
            Cmd.none

        | WindowFocusGained -> engine, Cmd.none
