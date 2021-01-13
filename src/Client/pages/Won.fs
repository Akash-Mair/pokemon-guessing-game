module Won


open Fable.React
open Fable.React.Props
open Fulma


type Model = { Lives: int }

type Msg = Restart

let init x = { Lives = x }

let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary
                Hero.IsFullHeight
                Hero.Props [ Style [ Background
                                         """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://i.pinimg.com/originals/28/d0/23/28d0231a25fc8a7ee02a9081251c47bf.png")""" ] ] ] [

        Hero.body [] [
            Container.container [ Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                  Container.IsWideScreen ] [
                Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsPrimary ] ] [
                    str (sprintf "You named all the Gen 1 Pokemon! With %i live(s) left :D" model.Lives)
                ]
                Button.button [ Button.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                Button.Size IsLarge
                                Button.Color IsPrimary
                                Button.OnClick(fun _ -> Restart |> dispatch) ] [
                    str "Restart"
                ]
            ]
        ]
    ]
