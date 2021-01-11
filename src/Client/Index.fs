module Index

open Elmish
open Shared
open Api

type PokemonForm = { IdInput: string; NameInput: string }

type DisplayedPokemon =
    | GuessedPokemon of Pokemon
    | NotGuessedPokemon of NotGuessedPokemon.Model

type Playing =
    { Pokemons: DisplayedPokemon list
      Score: int
      Lives: int }

type GameState =
    | StartScreen
    | Playing of Playing
    | EndGame of score: int

type Model = { GameState: GameState }

type PlayingMsg =
    | ShowPokemon of Pokemon
    | WrongGuess of exn
    | GuessPokemonMsg of NotGuessedPokemon.Msg

type Msg =
    | PlayingMsg of PlayingMsg
    | StartClicked
    | GameOver of score: int

let initGame () =
    { Pokemons = [ for i in 1 .. 151 -> NotGuessedPokemon <| NotGuessedPokemon.init i ]
      Lives = 3
      Score = 0 }

let init (): Model * Cmd<Msg> =
    let model = { GameState = StartScreen }

    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | StartClicked -> { GameState = Playing(initGame ()) }, Cmd.none

    | PlayingMsg gameMsg ->
        match model.GameState with
        | Playing game ->
            match gameMsg with
            | ShowPokemon poke ->
                let pokemons =
                    game.Pokemons
                    |> List.mapi
                        (fun i x ->
                            if (i + 1 = poke.Id) then
                                poke |> GuessedPokemon
                            else
                                x)

                { GameState =
                      Playing
                          { game with
                                Pokemons = pokemons
                                Score = game.Score + 1 } },
                Cmd.none
            | WrongGuess exn ->
                match game.Lives with
                | 1 ->
                    { model with
                          GameState = EndGame game.Score },
                    Cmd.none
                | _ -> { GameState = Playing { game with Lives = game.Lives - 1 } }, Cmd.none
            | GuessPokemonMsg newMsg ->
                match newMsg with
                | NotGuessedPokemon.Submit guessed ->
                    model,
                    Cmd.OfAsync.either
                        pokemonApi.CheckPokemon
                        (guessed.Id, guessed.Input)
                        (ShowPokemon >> PlayingMsg)
                        (WrongGuess >> PlayingMsg)

                | NotGuessedPokemon.SetInput (id, value) ->
                    let pokemons =
                        game.Pokemons
                        |> List.mapi
                            (fun i x ->
                                if (i + 1 = id) then
                                    NotGuessedPokemon.update newMsg { Id = id; Input = value }
                                    |> NotGuessedPokemon
                                else
                                    x)

                    { GameState = Playing { game with Pokemons = pokemons } }, Cmd.none
    | GameOver score -> { model with GameState = EndGame score }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand score lives =
    Navbar.navbar [ Navbar.Color IsPrimary ] [
        Navbar.Item.div [] [
            Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                str (sprintf "Lives left: %i" lives)
            ]
        ]
        Navbar.End.div [] [

            Navbar.Item.div [] [
                Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                    str (sprintf "Score: %i" score)
                ]
            ]
        ]
    ]


let showPokemon pokemon =
    Column.column [ Column.Width(Screen.All, Column.Is5)
                    Column.Width(Screen.Desktop, Column.Is3)
                    Column.Width(Screen.Mobile, Column.IsFull) ] [
        Box.box' [] [
            Columns.columns [] [
                Column.column [ Column.Width(Screen.All, Column.Is5) ] [
                    Heading.h6 [ Heading.Modifiers [ Modifier.TextColor IsBlackBis ] ] [
                        str (sprintf "%i" pokemon.Id)
                    ]
                ]
                Column.column [] [
                    Heading.h6 [ Heading.Modifiers [ Modifier.TextColor IsBlackBis ] ] [
                        str pokemon.Name
                    ]
                ]
            ]
            Image.image [] [
                img [ Src pokemon.ImageUrl ]
            ]
        ]
    ]


let gameOverView (score: int) =
    Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
        str (score |> string)
    ]




let view (model: Model) (dispatch: Msg -> unit) =

    match model.GameState with
    | StartScreen ->
        Hero.hero [ Hero.IsFullHeight
                    Hero.Props [ Style [ Background
                                             """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/d20d9193-3ffa-4a3a-915d-5312204873a7/d57lvld-b530ad43-7a5f-4be9-bd2b-0ffd9a83983d.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOiIsImlzcyI6InVybjphcHA6Iiwib2JqIjpbW3sicGF0aCI6IlwvZlwvZDIwZDkxOTMtM2ZmYS00YTNhLTkxNWQtNTMxMjIwNDg3M2E3XC9kNTdsdmxkLWI1MzBhZDQzLTdhNWYtNGJlOS1iZDJiLTBmZmQ5YTgzOTgzZC5qcGcifV1dLCJhdWQiOlsidXJuOnNlcnZpY2U6ZmlsZS5kb3dubG9hZCJdfQ.i5murD_hh7VxoNNDgB4W6SEGteWLAAv058ypRGBvVVU")""" ] ] ] [
            Hero.body [] [
                Columns.columns [ Columns.IsCentered
                                  Columns.IsVCentered ] [
                    Column.column [ Column.Width(Screen.All, Column.Is1) ] [
                        Button.button [ Button.Size IsLarge
                                        Button.Color IsPrimary
                                        Button.OnClick(fun _ -> StartClicked |> dispatch) ] [
                            str "Start"
                        ]
                    ]
                ]
            ]
        ]

    | Playing game ->
        Hero.hero [ Hero.Color IsPrimary
                    Hero.IsFullHeight
                    Hero.Props [ Style [ Background
                                             """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://i.pinimg.com/originals/28/d0/23/28d0231a25fc8a7ee02a9081251c47bf.png")""" ] ] ] [
            Hero.head [] [
                navBrand game.Score game.Lives
            ]
            Hero.body [] [
                Container.container [] [
                    Column.column [] [
                        Columns.columns [ Columns.IsMultiline ] [
                            for pokemon in game.Pokemons ->
                                match pokemon with
                                | NotGuessedPokemon x ->
                                    NotGuessedPokemon.view x (GuessPokemonMsg >> PlayingMsg >> dispatch)
                                | GuessedPokemon poke -> showPokemon poke
                        ]
                    ]
                ]
            ]
        ]


    | EndGame score ->
        Hero.hero [ Hero.IsFullHeight
                    Hero.Props [ Style [ Background
                                             """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://ninfora.com/forums/uploads/monthly_2016_09/2d6eba2c-e254-4271-b7ca-25f5a96ef4e0.png.5c7faa57f79402eccf50c4cc2e47b0e0.png")""" ] ] ] [
            Hero.body [] [
                Columns.columns [ Columns.IsCentered
                                  Columns.IsVCentered ] [
                    Column.column [] [
                        Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                            str (sprintf "Score: %i" score)
                        ]
                        Button.button [ Button.Color IsDanger
                                        Button.Size IsLarge
                                        Button.OnClick(fun _ -> StartClicked |> dispatch) ] [
                            str "Restart"
                        ]
                    ]
                ]
            ]
        ]
