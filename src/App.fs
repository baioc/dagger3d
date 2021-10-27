module App

open Feliz
open Feliz.Bulma
open Elmish
open Elmish.React


type Model = Model of int

type Msg = Increment

let init start =
    Model start, Cmd.none

let update msg model =
    let (Model count) = model
    match msg with
    | Increment -> Model (count + 1), Cmd.none

let view model dispatch =
    let (Model count) = model
    Bulma.button.button [
        prop.text $"You clicked: %i{count} time(s)"
        prop.onClick (fun _ -> dispatch Increment)
    ]


Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.runWith 0
