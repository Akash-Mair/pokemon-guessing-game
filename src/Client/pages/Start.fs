module Start

open Fable.React
open Fable.React.Props
open Fulma

type Msg = StartGame


let view (dispatch: Msg -> Unit) =
    Hero.hero [ Hero.IsFullHeight
                Hero.Props [ Style [ Background
                                         """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/d20d9193-3ffa-4a3a-915d-5312204873a7/d57lvld-b530ad43-7a5f-4be9-bd2b-0ffd9a83983d.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOiIsImlzcyI6InVybjphcHA6Iiwib2JqIjpbW3sicGF0aCI6IlwvZlwvZDIwZDkxOTMtM2ZmYS00YTNhLTkxNWQtNTMxMjIwNDg3M2E3XC9kNTdsdmxkLWI1MzBhZDQzLTdhNWYtNGJlOS1iZDJiLTBmZmQ5YTgzOTgzZC5qcGcifV1dLCJhdWQiOlsidXJuOnNlcnZpY2U6ZmlsZS5kb3dubG9hZCJdfQ.i5murD_hh7VxoNNDgB4W6SEGteWLAAv058ypRGBvVVU")""" ] ] ] [
        Hero.body [] [
            Container.container [ Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                  Container.IsWideScreen ] [

                Button.button [ Button.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]
                                Button.Size IsLarge
                                Button.Color IsPrimary
                                Button.OnClick(fun _ -> StartGame |> dispatch) ] [
                    str "Start"
                ]
            ]
        ]
    ]
