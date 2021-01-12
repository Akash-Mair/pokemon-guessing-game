module EndGame

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

type Model = { Score: int }

type Msg = Restart

let init score = { Score = score }


let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.IsFullHeight
                Hero.Props [ Style [ Background
                                         """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://ninfora.com/forums/uploads/monthly_2016_09/2d6eba2c-e254-4271-b7ca-25f5a96ef4e0.png.5c7faa57f79402eccf50c4cc2e47b0e0.png")""" ] ] ] [

        Hero.body [] [
            Container.container [ Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                  Container.IsWideScreen ] [
                Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsDanger ] ] [
                    str (sprintf "Score: %i" model.Score)
                ]
                Button.button [ Button.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                Button.Size IsLarge
                                Button.Color IsDanger
                                Button.OnClick(fun _ -> Restart |> dispatch) ] [
                    str "Restart"
                ]
            ]
        ]
    ]
