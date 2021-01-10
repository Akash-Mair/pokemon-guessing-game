module NotGuessedPokemon

open Fulma
open Fable.React


type Model = { Input: string; Id: int }

type Msg =
    | SetInput of int * string
    | Submit of Model

let init id = { Input = ""; Id = id }



let update msg model =
    match msg with
    | SetInput (id, value) -> { model with Id = id; Input = value }
    | Submit x -> x


let view (model: Model) (dispatch: Msg -> unit) =
    Column.column [ Column.Width(Screen.Desktop, Column.Is4) ] [
        Box.box' [] [
            Heading.p [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                str (model.Id |> string)
            ]
            Field.div [ Field.IsGrouped ] [
                Control.p [ Control.IsExpanded ] [
                    Input.text [ Input.Value model.Input
                                 Input.Placeholder "Name"
                                 Input.OnChange(fun x -> (model.Id, x.Value) |> SetInput |> dispatch) ]
                ]
                Control.p [] [
                    Button.a [ Button.Color IsPrimary
                               Button.OnClick(fun _ -> model |> Submit |> dispatch) ] [
                        str "Guess"
                    ]
                ]
            ]
        ]
    ]
