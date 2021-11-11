module Game

open LanguagePrimitives
open System

open Elmish
open Engine


type Canvas with
    /// Draws a single, vertically centered, 1px-wide column.
    member this.DrawColumn (color: Color) (x: int<px>) (h: int<px>) : unit =
        let top = this.Height/2 - h/2
        let bottom = this.Height/2 + h/2
        this.DrawLine color x top x bottom


/// Game engine configuration.
let CFG = Config.Default

type Tile =
    | Empty
    | Wall of color:Color

    static member SIZE = 2<1> // a tile ocupies SIZExSIZE world units

/// Game map, made out of a grid of tiles in order to do efficient raycasting.
type Map = // XXX: could be `Tile[,]` if Fable supported multi-dimensional arrays
    { Tiles: Tile[]
      Width: int; Height: int } // dimensions are in tile units, not world units

    static member make (w: uint, h: uint) tiles =
        { Tiles = tiles; Width = int w; Height = int h }

    member this.Item(i, j) =
        this.Tiles.[i * this.Width + j]

type Player =
    { X: float; Y: float
      Dir: float<rad> }

    // Controller configuration.
    static member SPEED = 14.0<1/s>
    static member TURN = 2.5<rad/s>
    static member FOV = deg.toRad 60.0<deg>
    static member DRAG_THRESHOLD = 0.1
    static member DRAG_SCALING_X = float CFG.Width * 0.333
    static member DRAG_SCALING_Y = float CFG.Height * 0.666

type World =
    { Player: Player
      Map: Map
      ShowMinimap: bool
      LeftStick: ScreenTouch option }


/// Converts to float, preserving unit of measure.
let f<[<Measure>] 'u> (x: int<'u>): float<'u> =
    x |> float |> FloatWithMeasure

/// "Steps" point `x, y` a distance `len` towards a certain direction `dir`.
let step x y len (dir: float<rad>) =
    let x, y = float x, float y
    let dir = float dir
    let dy = len * sin dir
    let dx = len * cos dir
    x + dx, y + dy

let distance x1 y1 x2 y2 =
    sqrt ((x1 - x2)**2.0 + (y1 - y2)**2.0)


let init () =
    let x = 4.5 * float Tile.SIZE
    let y = 4.5 * float Tile.SIZE
    let o = Empty
    let W = Wall (rgb 0x41 0x41 0x41)
    let D = Wall (rgb 0x3a 0x82 0x83)
    { Map =
        Map.make
            (28u, 19u)
            [| W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; o; W; W; W; W; W; W; W; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; D; o; o; o; o; o; o; o; o; W; W; W; D; W; W; o; W; W; o; W; W; W; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; o; W; W; W; W; W; W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; W; W; W; o; W; W; o; W; W; D; W; W; W; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; W; o; o; o; o; o; o; o; o; o; o; o; W; o; o; o; o; o; W; o; o; o; o; o; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; o; W
               W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W; W |]
      ShowMinimap = false
      LeftStick = None
      Player = { X = x; Y = y; Dir = 0.0<rad> } }
    |> just


let update dt (game: Runtime<_>) =
    let player = game.State.Player
    let map = game.State.Map

    // joystick-style movement: speed forwards/backwards and steer left/right
    let dragX, dragY =
        match game.State.LeftStick with
        | None -> 0.0, 0.0
        | Some initial ->
            match game.Input.TryFindPressedTouch initial.Id with
            | None -> 0.0, 0.0
            | Some current ->
                let dx = (current.X - initial.X)
                let dy = (current.Y - initial.Y)
                Math.Clamp(float <| dx / Player.DRAG_SCALING_X, -1.0, +1.0),
                Math.Clamp(float <| dy / Player.DRAG_SCALING_Y, -1.0, +1.0)
    let up =
        if game.Input.IsKeyPressed "ArrowUp" || game.Input.IsKeyPressed "KeyW" then
            1.0
        elif dragY < -Player.DRAG_THRESHOLD then
            abs dragY
        else
            0.0
    let down =
        if game.Input.IsKeyPressed "ArrowDown" || game.Input.IsKeyPressed "KeyS" then
            1.0
        elif dragY > Player.DRAG_THRESHOLD then
            abs dragY
        else
            0.0
    let left =
        if game.Input.IsKeyPressed "ArrowLeft" || game.Input.IsKeyPressed "KeyA" then
            1.0
        elif dragX < -Player.DRAG_THRESHOLD then
            abs dragX
        else
            0.0
    let right =
        if game.Input.IsKeyPressed "ArrowRight" || game.Input.IsKeyPressed "KeyD" then
            1.0
        elif dragX > Player.DRAG_THRESHOLD then
            abs dragX
        else
            0.0
    let speed = up * Player.SPEED - down * Player.SPEED
    let speed = if speed < 0.0<1/s> then speed * 0.75 else speed
    let steer = right * Player.TURN - left * Player.TURN

    // scale by timestep
    let dist = speed / ms.perSecond * dt
    let turn = steer / ms.perSecond * dt

    // ad-hoc movement
    let dir = player.Dir + turn
    let dir = dir % (2.0<rad>*Math.PI)
    let x, y = step player.X player.Y dist dir
    let x = Math.Clamp(x, 0.0, float <| (map.Width - 1)  * Tile.SIZE)
    let y = Math.Clamp(y, 0.0, float <| (map.Height - 1) * Tile.SIZE)

    // check for map collisions
    let x, y =
        let i, j = int y / Tile.SIZE, int x / Tile.SIZE
        if j < 0 || j >= map.Width || i < 0 || i >= map.Height then
            player.X, player.Y
        else
            match map.[i, j] with
            | Empty -> x, y
            | Wall w -> player.X, player.Y

    let player = { player with Dir = dir; X = x; Y = y }
    just { game.State with Player = player  }


let handler event (game: Runtime<_>) =
    let state = game.State
    let hasLeftStick = Option.isSome state.LeftStick

    match event with
    // when a touch is detected on the left side of the screen, create a joystick
    | Input (TouchPressed { Changed = pressed }) ->
        let touch =
            pressed
            |> Map.tryFindKey (fun id touch -> int touch.X < CFG.Width/2<px>)
            |> Option.map (fun id -> Map.find id pressed)
        match touch with
        | Some touch when not hasLeftStick ->
            just { state with LeftStick = Some touch }
        | _ -> just state

    // unattach the left joystick when its finger is released
    | Input (TouchReleased { Changed = released }) when hasLeftStick ->
        let leftStick = Option.get state.LeftStick
        if Map.containsKey leftStick.Id released then
            just { state with LeftStick = None }
        else
            just state

    // toggle minimap ON
    | Input (KeyPressed { Code = "Tab" }) ->
        just { state with ShowMinimap = true }

    // toggle minimap OFF
    | Input (KeyReleased { Code = "Tab" }) ->
        just { state with ShowMinimap = false }

    | _ -> just state


let draw state (canvas: Canvas) =
    let player = state.Player
    let map = state.Map
    let px, py = player.X, player.Y
    let w, h = canvas.Width, canvas.Height

    // background (floor and ceiling)
    canvas.DrawRect (rgb 0 0 0)          0<px> 0<px> w (h/2)
    canvas.DrawRect (rgb 0x2d 0x26 0x1c) 0<px> (h/2) w (h/2)

    // DDA raycasting
    for column in 0 .. int w - 1 do
        let column = column * 1<px>

        // player-relative angle to cast the ray
        let angle =
            let radPerPixel = Player.FOV / f w
            let pixelsFromCenter = column - w/2
            player.Dir + radPerPixel * (f pixelsFromCenter)
        let angle = angle % (2.0<rad>*Math.PI)

        // to find the first grid-aligned intersections, we solve the system
        // line equation twice, for known x' and y' such that if m = dy/dx,
        // then y' = py + m * (x' - px) and x' = px + (1/m) * (y' - py)
        let dx, dy =
            let tx, ty = step px py (float <| 2*Tile.SIZE) angle
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
        let gridX, gridY = float (Tile.SIZE * sign dx), float (Tile.SIZE * sign dy)
        let stepY = float Tile.SIZE * tan (float angle)
        let stepY = abs stepY * float (sign dy)
        let stepX = float Tile.SIZE * tan (Math.PI/2.0 - float angle)
        let stepX = abs stepX * float (sign dx)
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
        | None -> ()
        | Some (position, tile) ->
            match map.[tile] with
            | Empty -> ()
            | Wall color ->
                let distance = distance px py <|| position
                let fishEyeCorrected = distance * cos (float (angle - player.Dir))
                let height = 1024.0 / fishEyeCorrected
                canvas.DrawColumn color column (int height * 1<px>)

    // minimap
    if state.ShowMinimap then
        let minimapScale = 2<px>
        for row in 0 .. map.Height - 1 do
            for col in 0 .. map.Width - 1 do
                match map.[row, col] with
                | Empty -> ()
                | Wall color ->
                    let tileSize = Tile.SIZE * minimapScale
                    let color = rgba (color.R + 64) (color.G + 64) (color.B + 64) 0.5
                    canvas.DrawRect color (col * tileSize) (row * tileSize) tileSize tileSize

        canvas.DrawRect (rgba 0 255 0 0.5) (int px * minimapScale) (int py * minimapScale) (2 * minimapScale) (2 * minimapScale)
        let tx, ty = step px py (6.0 * float minimapScale) player.Dir
        canvas.DrawLine (rgba 0 255 0 0.5) (int px * minimapScale) (int py * minimapScale) (int tx * minimapScale) (int ty * minimapScale)


Game.make init update handler draw
|> Game.withConfig CFG
|> Game.run
