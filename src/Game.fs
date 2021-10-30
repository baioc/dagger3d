/// Ad hoc game logic that is fed into the engine when the app starts.
module Game

open Elmish

open Engine
open Engine.Input


type Model = Model of int

type Msg = Nop

let init () =
    Model 0, Cmd.ofCustom Nop

let update dt model =
    model.State, Cmd.none

let handler event model =
    let (Model t) = model.State
    Model (t + 1), Cmd.none

let draw model (ctx: Canvas) =
    ctx.fillStyle <- !^ (rgba 255 0 255 0.5)
    ctx.fillRect(0.0, 0.0, float ctx.Width, float ctx.Height)


let game =
    { Init = init; Update = update; Handler = handler; Draw = draw }
