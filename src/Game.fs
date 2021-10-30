/// Ad hoc game logic that is fed into the engine when the app starts.
module Game

open System
open LanguagePrimitives

open Browser
open Browser.Types
open Elmish

open Engine
open Engine.Input


type CanvasRenderingContext2D with
    /// Draws a single, vertically centered, 1px-wide column.
    member this.drawColumn(x: int, columnHeight: int) =
        this.save()
        this.lineWidth <- 1.0
        this.lineCap <- "butt"
        this.moveTo(x, this.height - columnHeight/2)
        this.lineTo(x, this.height + columnHeight/2)
        this.restore()


type Tile =
    | Empty
    | Wall of color:string

    static member size = 8

[<Struct>]
type Map =
    { Tiles: Tile[]
      Width: int; Height: int}

    member this.Item(i: int, j: int) : Tile =
        this.Tiles.[i * this.Width + j]

    static member make (w: int, h: int) (tiles: Tile[]) =
        { Tiles = tiles; Width = w; Height = h }

type Player =
    { X: int; Y: int
      Dir: float<rad> }

    static member speed = 16<1/s>
    static member turn = 2<1/s>

type World =
    { Map: Map
      Player: Player }

let init () =
    let e = Empty
    let w = Wall (rgb 0x41 0x41 0x41)
    let d = Wall (rgb 0x3a 0x82 0x83)
    { Map =
        Map.make
            (6, 6)
            [| w; w; w; w; w; w
               w; e; e; e; e; w
               w; e; e; e; e; d
               w; e; e; e; e; w
               w; w; w; w; w; w
               e; e; e; e; e; e |]
      Player = { X = 20; Y = 20; Dir = 0.0<rad> } },
    Cmd.none


let step len (dir: float<rad>) x y =
    let dy = len * Math.Sin(float dir) |> Math.Round |> int
    let dx = len * Math.Cos(float dir) |> Math.Round |> int
    x + dx, y + dy


let update dt game =
    let dt = ms.toSecond dt
    let isKeyPressed = game.Input.IsKeyPressed
    let player = game.State.Player

    // joystick-style controls move forwards/backwards and steer left/right
    let speed = 0<1/s>
    let speed = if isKeyPressed "ArrowUp" || isKeyPressed "KeyW" then speed + Player.speed else speed
    let speed = if isKeyPressed "ArrowDown" || isKeyPressed "KeyS" then speed - Player.speed else speed
    let speed = if speed < 0<1/s> then speed / 2 else speed
    let steer = 0<1/s>
    let steer = if isKeyPressed "ArrowLeft" || isKeyPressed "KeyA" then steer - Player.turn else steer
    let steer = if isKeyPressed "ArrowRight" || isKeyPressed "KeyD" then steer + Player.turn else steer

    let dist = (speed |> float |> FloatWithMeasure) * dt
    let steer = 1.0<rad> * float steer * float dt

    // apply movement
    let dir = player.Dir + steer
    let x, y = step dist dir player.X player.Y
    let player = { player with Dir = dir; X = x; Y = y }

    { game.State with Player = player  }, Cmd.none


let handler event game =
    game.State, Cmd.none


let draw state (ctx: Canvas) =
    let w, h = ctx.width, ctx.height
    let player = state.Player

    // background (floor and ceiling)
    ctx.color <- rgb 0 0 0
    ctx.fillRect(0, 0, w, h / 2)
    ctx.color <- rgb 0x2d 0x26 0x1c
    ctx.fillRect(0, h / 2, w, h / 2)

    // raycasting
    ctx.color <- rgb 255 0 0
    ctx.fillRect(player.X, player.Y, 4, 4)
    ctx.beginPath()
    ctx.moveTo(player.X, player.Y)
    let x', y' = step 16.0 player.Dir player.X player.Y
    ctx.lineTo(x', y')
    ctx.stroke()


let game =
    { Init = init; Update = update; Handler = handler; Draw = draw }
