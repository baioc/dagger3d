namespace Engine

open Elmish


/// Elmish wrapper mixing engine and custom messages. See `Cmd` extension.
type GameEvent<'custom> =
    | Input of InputEvent
    | Custom of 'custom

[<RequireQualifiedAccess>]
module Cmd =
    let ofCustom (msg: 'msg) : Cmd<GameEvent<'msg>> =
        msg |> Custom |> Cmd.ofMsg

    let ofInput (msg: InputEvent) : Cmd<GameEvent<'msg>> =
        msg |> Input |> Cmd.ofMsg

[<AutoOpen>]
module Elmish =
    let just (x: 'model) : 'model * Cmd<'msg> =
        x, Cmd.none

/// Wraps the game state with some extra runtime information.
type Runtime<'game> =
    abstract member State: 'game
    abstract member Input: Input

type Game<'model, 'msg> =
    abstract member Init: unit -> 'model * Cmd<GameEvent<'msg>>
    abstract member Update: float<ms> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>
    abstract member Handler: GameEvent<'msg> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>
    abstract member Draw: 'model -> Canvas -> unit


/// For library authors only.
[<RequireQualifiedAccess>]
module Internal =

    // we consider engine states to be equal when the game state is equal
    [<CustomEquality; NoComparison>]
    type Engine<'state, 'event when 'state: equality> =
        { PressedKeys: Set<string> // stores `Code`s, not `Key`s (despite the name)
          PressedButtons: Set<MouseButton>
          PressedTouches: Map<TouchId, ScreenTouch>
          Game: Game<'state, 'event>
          GameState: 'state }

        override this.Equals(other) =
            match other with
            | :? Engine<'state, 'event> as other -> this.GameState = other.GameState
            | _ -> false

        override this.GetHashCode() =
            this.GameState.GetHashCode()

        interface Input with
            override this.IsKeyPressed key = Set.contains key this.PressedKeys
            override this.IsMouseButtonPressed btn = Set.contains btn this.PressedButtons
            override this.TryFindPressedTouch tch = Map.tryFind tch this.PressedTouches

        interface Runtime<'state> with
            override this.State = this.GameState
            override this.Input = this :> Input


    type EngineEvent<'custom> =
        | Tick of dt:float<ms>
        | GameEvent of GameEvent<'custom>

    let load (game: Game<_, _>) =
        let state, cmd = game.Init()
        { PressedKeys = Set.empty
          PressedButtons = Set.empty
          PressedTouches = Map.empty
          Game = game
          GameState = state },
        Cmd.map GameEvent cmd

    let handleEvent event engine =
        match event with
        | Tick dt ->
            let state, cmd = engine.Game.Update dt engine
            { engine with GameState = state }, Cmd.map GameEvent cmd

        | GameEvent event ->
            let engine =
                match event with
                | Input (KeyPressed key) ->
                    { engine with
                        PressedKeys = Set.add key.Code engine.PressedKeys }
                | Input (KeyReleased key) ->
                    { engine with
                        PressedKeys = Set.remove key.Code engine.PressedKeys }
                | Input (MousePressed btn) ->
                    { engine with
                        PressedButtons = Set.add btn.Button engine.PressedButtons }
                | Input (MouseReleased btn) ->
                    { engine with
                        PressedButtons = Set.remove btn.Button engine.PressedButtons }
                | Input (TouchPressed { Changed = pressed }) ->
                    { engine with
                        PressedTouches =
                            Map.toSeq engine.PressedTouches
                            |> Seq.append (Map.toSeq pressed)
                            |> Map.ofSeq }
                | Input (TouchReleased { Changed = released }) ->
                    { engine with
                        PressedTouches =
                            engine.PressedTouches
                            |> Map.filter (fun id _ -> not (Map.containsKey id released)) }
                | Input (TouchMoved { Changed = moved }) ->
                    { engine with
                        PressedTouches =
                            engine.PressedTouches
                            |> Map.map
                                (fun id previous ->
                                    Map.tryFind id moved
                                    |> Option.defaultValue previous) }
                | Input (MouseMoved _)
                | Input (MouseWheelRotated _)
                | Custom _ -> engine

            let state, cmd = engine.Game.Handler event engine
            { engine with GameState = state }, Cmd.map GameEvent cmd


/// Runtime configuration.
[<Struct>]
type Config =
    { Resolution: int<px> * int<px>
      FPS: float }

    member this.Width: int<px> = fst this.Resolution
    member this.Height: int<px> = snd this.Resolution

    static member Default = {
        Resolution = 640<px>, 360<px>
        FPS =
            #if DEBUG
                10.0
            #else
                30.0
            #endif
    }


[<RequireQualifiedAccess>]
module Game =
    type Instance<'model, 'msg> = internal {
        Game: Game<'model, 'msg>
        Config: Config
    }

    /// Creates a game with default configs.
    let make
            (init: unit -> 'model * Cmd<GameEvent<'msg>>)
            (update: float<ms> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>)
            (handler: GameEvent<'msg> -> Runtime<'model> -> 'model * Cmd<GameEvent<'msg>>)
            (draw: 'model -> Canvas -> unit) =
        { Game =
            { new Game<'model, 'msg> with
                override __.Init () = init ()
                override __.Update dt game = update dt game
                override __.Handler event game = handler event game
                override __.Draw state ctx = draw state ctx }
          Config = Config.Default }

    let withConfig (config: Config) game =
        { game with Config = config }
