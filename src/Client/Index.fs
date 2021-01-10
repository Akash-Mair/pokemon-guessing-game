module Index

open Elmish
open Shared
open Api

type PokemonForm = { IdInput: string; NameInput: string }

type DisplayedPokemon =
    | GuessedPokemon of Pokemon
    | NotGuessedPokemon of NotGuessedPokemon.Model

// type GameState =
//     | StartScreen
//     | Playing of Playing.Page
//     | GameOver of score : int

// type Model = {
//     GameState: GameState
// }


type Model =
    { Pokemons: DisplayedPokemon list
      Lives: int }

type Msg =
    | ShowPokemon of Pokemon
    | WrongGuess of exn
    | GuessPokemonMsg of NotGuessedPokemon.Msg



let init (): Model * Cmd<Msg> =
    let model =
        { Pokemons = [ for i in 1 .. 151 -> NotGuessedPokemon <| NotGuessedPokemon.init i ]
          Lives = 3 }

    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | ShowPokemon poke ->

        let pokemons =
            model.Pokemons
            |> List.mapi
                (fun i x ->
                    if (i + 1 = poke.Id) then
                        poke |> GuessedPokemon
                    else
                        x)

        { model with Pokemons = pokemons }, Cmd.none
    | WrongGuess exn -> { model with Lives = model.Lives - 1 }, Cmd.none
    | GuessPokemonMsg msg ->
        match msg with
        | NotGuessedPokemon.Submit guessed ->
            model, Cmd.OfAsync.either pokemonApi.CheckPokemon (guessed.Id, guessed.Input) ShowPokemon WrongGuess

        | NotGuessedPokemon.SetInput (id, value) ->
            let pokemons =
                model.Pokemons
                |> List.mapi
                    (fun i x ->
                        if (i + 1 = id) then
                            NotGuessedPokemon.update msg { Id = id; Input = value }
                            |> NotGuessedPokemon
                        else
                            x)

            { model with Pokemons = pokemons }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand lives =
    Navbar.navbar [ Navbar.Color IsInfo ] [
        Navbar.Item.a [] [
            Navbar.Link.a [] [ str "Docs" ]
        ]
        Navbar.End.div [] [
            Navbar.Item.div [] [
                Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                    str (lives |> string)
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




let view (model: Model) (dispatch: Msg -> unit) =

    Hero.hero [ Hero.Color IsPrimary
                Hero.IsFullHeight
                Hero.Props [ Style [ Background
                                         """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://i.pinimg.com/originals/28/d0/23/28d0231a25fc8a7ee02a9081251c47bf.png")""" ] ] ] [
        Hero.head [] [
            Navbar.navbar [] [
                Heading.h1 [ Heading.Modifiers [ Modifier.TextColor IsBlack ] ] [
                    str (model.Lives |> string)
                ]
                Container.container [] [
                    Column.column [] [
                        Columns.columns [ Columns.IsMultiline ] [
                            for pokemon in model.Pokemons ->
                                match pokemon with
                                | NotGuessedPokemon x -> NotGuessedPokemon.view x (GuessPokemonMsg >> dispatch)
                                | GuessedPokemon poke -> showPokemon poke
                        ]
                    ]
                ]
            ]
        ]
    ]
