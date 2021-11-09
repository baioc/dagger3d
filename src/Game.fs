/// Ad hoc game logic that is fed into the engine when the app starts.
module Game

open System
open LanguagePrimitives

open Browser.Types
open Elmish

open Engine


type CanvasRenderingContext2D with
    /// Draws a single, vertically centered, 1px-wide column.
    member this.drawColumn(column: int, columnHeight: int) =
        this.lineWidth <- 1.0
        this.beginPath()
        this.moveTo(column, this.height/2 - columnHeight/2)
        this.lineTo(column, this.height/2 + columnHeight/2)
        this.stroke()


type Tile =
    | Empty
    | Wall of color:string

    static member SIZE = 8<1> // a tile ocupies SIZExSIZE world units

/// Game map, made out of a grid of tiles in order to do DDA raycasting.
[<Struct>]
type Map =
    { Tiles: Tile[]
      Width: int; Height: int } // dimensions are in tile units, not world units

    static member make (w, h) tiles =
        { Tiles = tiles; Width = w; Height = h }

    member this.Item(i, j) =
        this.Tiles.[i * this.Width + j]

type Player =
    { X: float; Y: float
      Dir: float<rad> }

    static member SPEED = 32.0<1/s>
    static member TURN = 2.0<rad/s>
    static member FOV = deg.toRad 90.0<deg>

type World =
    { Map: Map
      Player: Player }


/// Converts to float, preserving unit of measure.
let inline f<[<Measure>] 'u> (x: int<'u>): float<'u> =
    x |> float |> FloatWithMeasure

/// "Steps" point `x, y` a distance `len` towards a certain direction `dir`.
let inline step x y len (dir: float<rad>) =
    let x = float x
    let y = float y
    let len = float len
    let dy = len * Math.Sin(float dir)
    let dx = len * Math.Cos(float dir)
    x + dx, y + dy

let inline distance x1 y1 x2 y2 =
    let x1 = float x1
    let y1 = float y1
    let x2 = float x2
    let y2 = float y2
    Math.Sqrt((x1 - x2)**2.0 + (y1 - y2)**2.0)


let init () =
    let x = 4.5 * float Tile.SIZE
    let y = 4.5 * float Tile.SIZE
    let o = Empty
    let W = Wall (rgb 0x41 0x41 0x41)
    let D = Wall (rgb 0x3a 0x82 0x83)
    { Map =
        Map.make
            (21, 13)
            [| W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; W; W; W; W; W; W; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; W; o; o; o; o; o; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; D; o; o; o; o; o; o; o; W; W; D; W; o; W; D; W; W; o; W
               W; o; W; o; o; o; o; o; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; W; W; W; W; W; W; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; W; o; o; o; o; o; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; W; o; o; o; o; o; o; o; W; W; D; W; o; W; D; W; W; o; W
               W; o; W; o; o; o; o; o; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; W; o; o; o; o; o; o; o; o; o; W; o; o; o; W; o; o; o; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W |]
      Player = { X = x; Y = y; Dir = 0.0<rad> } },
    Cmd.none


let update dt game =
    let isKeyPressed = game.Input.IsKeyPressed
    let player = game.State.Player
    let map = game.State.Map

    // joystick-style controls: move forwards/backwards and steer left/right
    let speed = 0.0<1/s>
    let speed = if isKeyPressed "ArrowUp" || isKeyPressed "KeyW" then speed + Player.SPEED else speed
    let speed = if isKeyPressed "ArrowDown" || isKeyPressed "KeyS" then speed - Player.SPEED else speed
    let speed = if speed < 0.0<1/s> then speed*0.5 else speed
    let steer = 0.0<rad/s>
    let steer = if isKeyPressed "ArrowLeft" || isKeyPressed "KeyA" then steer - Player.TURN else steer
    let steer = if isKeyPressed "ArrowRight" || isKeyPressed "KeyD" then steer + Player.TURN else steer

    // scale by timestep
    let dist = speed / ms.perSecond * dt
    let turn = steer / ms.perSecond * dt

    // ad-hoc movement
    let dir = player.Dir + turn
    let dir = dir % (2.0<rad>*Math.PI)
    let x, y = step player.X player.Y dist dir
    let x = Math.Clamp(x, 0.0, float <| (map.Width - 1)  * Tile.SIZE)
    let y = Math.Clamp(y, 0.0, float <| (map.Height - 1) * Tile.SIZE)

    // check for collisions
    let x, y =
        let i, j = int y / Tile.SIZE, int x / Tile.SIZE
        if j < 0 || j >= map.Width || i < 0 || i >= map.Height then
            player.X, player.Y
        else
            match map.[i, j] with
            | Empty -> x, y
            | Wall w -> player.X, player.Y

    let player = { player with Dir = dir; X = x; Y = y }
    { game.State with Player = player  }, Cmd.none


let handler event game =
    game.State, Cmd.none


let draw state (ctx: Canvas) =
    let w, h = ctx.width, ctx.height
    let player = state.Player
    let px, py, dir = player.X, player.Y, player.Dir
    let fov = Player.FOV
    let map = state.Map

    // background (floor and ceiling)
    ctx.color <- rgb 0 0 0
    ctx.fillRect(0, 0, w, h / 2)
    ctx.color <- rgb 0x2d 0x26 0x1c
    ctx.fillRect(0, h / 2, w, h / 2)

    // DDA raycasting
    for column in 0 .. w - 1 do
        // player-relative angle to cast the ray
        let angle =
            let radPerPixel = fov / f (w * 1<px>)
            let pixelsFromCenter = (column - w/2) * 1<px>
            dir + radPerPixel * (f pixelsFromCenter)
        let angle = angle % (2.0<rad>*Math.PI)

        // to find the first grid-aligned intersections, we solve the system
        // line equation twice, for known x' and y' such that if m = dy/dx,
        // then y' = py + m * (x' - px) and x' = px + (1/m) * (y' - py)
        let dx, dy =
            let tx, ty = step px py (2*Tile.SIZE) angle
            tx - px, ty - py
        let intersectX =
            let x' = px - (px % float Tile.SIZE) + (if dx > 0.0 then float Tile.SIZE else 0.0)
            let y' = py + (dy/dx) * (x' - px)
            x', y'
        let intersectY =
            let y' = py - (py % float Tile.SIZE) + (if dy > 0.0 then float Tile.SIZE else 0.0)
            let x' = px + (dx/dy) * (y' - py)
            x', y'

        // march the ray forward until something is hit (or map limit reached)
        let gridX, gridY = float (Math.Sign(dx)*Tile.SIZE), float (Math.Sign(dy)*Tile.SIZE)
        let stepY = float Tile.SIZE * Math.Tan(float angle)
        let stepY = Math.Abs(stepY) * float (Math.Sign(dy))
        let stepX = float Tile.SIZE * Math.Tan(Math.PI/2.0 - float angle)
        let stepX = Math.Abs(stepX) * float (Math.Sign(dx))
        let rec rayMarch x y intersectX intersectY =
            // move to the closest grid intersection point
            let distX = distance x y <|| intersectX
            let distY = distance x y <|| intersectY
            let x, y, intersectX, intersectY, dir =
                if distX < distY then
                    let x', y' = intersectX
                    x', y', (x' + gridX, y' + stepY), intersectY, Choice1Of2 ()
                else
                    let x', y' = intersectY
                    x', y', intersectX, (x' + stepX, y' + gridY), Choice2Of2 ()
            // check for a hit, rounding based on the step direction
            let i, j =
                let x, y = int x, int y
                match dir with
                | Choice1Of2 () -> y / Tile.SIZE, x / Tile.SIZE + (if dx < 0.0 then -1 else 0)
                | Choice2Of2 () -> y / Tile.SIZE + (if dy < 0.0 then -1 else 0), x / Tile.SIZE
            if j < 0 || j >= map.Width || i < 0 || i >= map.Height then
                None
            else
                match map.[i, j] with
                | Wall _ as w -> Some ((x, y), (i, j))
                | Empty -> rayMarch x y intersectX intersectY
        let hit = rayMarch px py intersectX intersectY

        // draw a single vertical column based on what was hit
        match hit with
        | Some (position, tile) ->
            match map.[tile] with
            | Empty -> ()
            | Wall color ->
                let distance = distance px py <|| position
                let fishEyeCorrected = distance * cos (float (dir - angle))
                let height = 2048.0 / fishEyeCorrected
                ctx.color <- color
                ctx.drawColumn(column, int height)
        | None -> ()

    // minimap
    ctx.save()
    ctx.globalAlpha <- 0.5
    ctx.globalCompositeOperation <- "lighter"
    for row in 0 .. map.Height - 1 do
        for col in 0 .. map.Width - 1 do
            match map.[row, col] with
            | Empty -> ()
            | Wall color ->
                ctx.color <- color
                ctx.fillRect(col * Tile.SIZE, row * Tile.SIZE, Tile.SIZE, Tile.SIZE)
    ctx.restore()

    // "miniplayer"
    ctx.color <- rgba 0 255 0 0.5
    ctx.fillRect(px, py, 4.0, 4.0)
    ctx.beginPath()
    ctx.moveTo(px, py)
    let tx, ty = step px py 16.0 dir in ctx.lineTo(tx, ty)
    ctx.stroke()


let game =
    { Init = init; Update = update; Handler = handler; Draw = draw }
