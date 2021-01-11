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
        | EndGame score -> { model with GameState = EndGame score }, Cmd.none
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
        Button.button [ Button.OnClick(fun _ -> StartClicked |> dispatch) ] [
            str "Start"
        ]

    | Playing game ->
        Hero.hero [ Hero.IsFullHeight
                    Hero.Props [ Style [ Background
                                             """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://i.pinimg.com/originals/28/d0/23/28d0231a25fc8a7ee02a9081251c47bf.png")""" ] ] ] [
            Hero.head [] [
                navBrand game.Score game.Lives
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
        div [] [
            Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                str (score |> string)
            ]
            Button.button [ Button.OnClick(fun _ -> StartClicked |> dispatch) ] [
                str "Restart"
            ]
        ]
