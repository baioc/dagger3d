namespace Engine


module Input =

    [<AutoOpen>]
    module Keyboard =
        /// Physical (`Code`) and logical (`Key`) key. See https://keycode.info/
        [<Struct>]
        type KeyboardInput =
            { Code: string; Key: string }

        type KeyboardEvent =
            | KeyPressed of key:KeyboardInput
            | KeyReleased of key:KeyboardInput


    [<AutoOpen>]
    module Mouse =
        [<Struct>]
        type MouseButton =
            | LeftClick
            | MiddleClick
            | RightClick

        [<Struct>]
        type MouseInput =
            { Button: MouseButton
              X: float<px>; Y: float<px> }

        [<Struct>]
        type MouseMovement =
            { Dx: float<px>; Dy: float<px>
              X: float<px>; Y: float<px> }

        [<Struct>]
        type MouseWheelRotation =
            { Delta: float }

        type MouseEvent =
            | MousePressed of button:MouseInput
            | MouseReleased of button:MouseInput
            | MouseMoved of MouseMovement
            | MouseWheelRotated of MouseWheelRotation


    [<AutoOpen>]
    module Touch =
        type TouchId = int64

        [<Struct>]
        type ScreenTouch =
            { Id: TouchId
              X: float<px>; Y: float<px> }

        [<Struct>]
        type TouchInput =
            { Changed: Map<TouchId, ScreenTouch> }

        type TouchEvent =
            | TouchPressed of touch:TouchInput
            | TouchReleased of touch:TouchInput
            | TouchMoved of touch:TouchInput


    type InputEvent =
        | KeyboardEvent of KeyboardEvent
        | MouseEvent of MouseEvent
        | TouchEvent of TouchEvent


open Input

/// Runtime input information.
type Input =
    { IsKeyPressed: string -> bool
      IsMouseButtonPressed: MouseButton -> bool
      IsTouchPressed: TouchId -> bool }
