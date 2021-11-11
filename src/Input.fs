namespace Engine


[<AutoOpen>]
module Input =

    /// Physical (`Code`) and logical (`Key`) key. See https://keycode.info/
    [<Struct>]
    type KeyboardInput =
        { Code: string; Key: string }


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


    type TouchId = int64

    [<Struct>]
    type ScreenTouch =
        { Id: TouchId
          X: float<px>; Y: float<px> }

    [<Struct>]
    type TouchInput =
        { Changed: Map<TouchId, ScreenTouch> }


    type InputEvent =
        | KeyPressed of key:KeyboardInput
        | KeyReleased of key:KeyboardInput
        | MousePressed of button:MouseInput
        | MouseReleased of button:MouseInput
        | MouseMoved of MouseMovement
        | MouseWheelRotated of MouseWheelRotation
        | TouchPressed of touch:TouchInput
        | TouchReleased of touch:TouchInput
        | TouchMoved of touch:TouchInput

/// Runtime input information.
type Input =
    abstract member IsKeyPressed: keyCode:string -> bool
    abstract member IsMouseButtonPressed: button:MouseButton -> bool
    abstract member TryFindPressedTouch: touchId:TouchId -> ScreenTouch option
