module Playing

open Shared

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Api

type DisplayedPokemon =
    | GuessedPokemon of Pokemon
    | NotGuessedPokemon

type Model =
    { Pokemons: DisplayedPokemon list
      Score: int
      Input: string
      Lives: int }

type Msg =
    | ShowPokemon of Pokemon
    | WrongGuess of exn
    | SetInput of string
    | Submit


let init () =
    { Pokemons = [ for i in 1 .. 151 -> NotGuessedPokemon ]
      Input = ""
      Lives = 3
      Score = 0 },
    Cmd.none

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

        { model with
              Pokemons = pokemons
              Input = ""
              Score = model.Score + 1 },
        Cmd.none

    | WrongGuess exn ->
        { model with
              Lives = model.Lives - 1
              Input = "" },
        Cmd.none
    | SetInput x -> { model with Input = x }, Cmd.none
    | Submit -> model, Cmd.OfAsync.either pokemonApi.CheckPokemon model.Input ShowPokemon WrongGuess


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


let notGuessedPokemon =
    Column.column [ Column.Width(Screen.All, Column.Is5)
                    Column.Width(Screen.Desktop, Column.Is3)
                    Column.Width(Screen.Mobile, Column.IsFull) ] [
        Box.box' [] [
            Columns.columns [] [
                Column.column [ Column.Width(Screen.All, Column.Is5) ] [
                    Heading.h6 [ Heading.Modifiers [ Modifier.TextColor IsBlackBis ] ] [
                        str "Not Guessed"
                    ]
                ]
            ]
            Image.image [] [
                img [ Src
                          "https://lh3.googleusercontent.com/proxy/v8tvZ1QhUoc0GBuhHpayeP7YDUvFEUi9WNrZzqXYPb8e_h94Ho39N05f0MoJC_NyLdxVHHDAPrOh19OLK8TvAdHnBvCo2RxRZuEpReep55WdsfFE7MC6JqWdE-lyJHpPPGTBcz3s2eWC" ]
            ]
        ]
    ]


let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary
                Hero.IsFullHeight
                Hero.Props [ Style [ Background
                                         """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://i.pinimg.com/originals/28/d0/23/28d0231a25fc8a7ee02a9081251c47bf.png")""" ] ] ] [
        Hero.head [] [
            navBrand model.Score model.Lives
        ]
        Hero.body [] [
            Container.container [] [

                Field.div [ Field.IsGrouped ] [
                    Control.p [ Control.IsExpanded ] [
                        Input.text [ Input.Value model.Input
                                     Input.Placeholder "Name"
                                     Input.OnChange(fun x -> x.Value |> SetInput |> dispatch) ]
                    ]
                    Control.p [] [
                        Button.a [ Button.Color IsPrimary
                                   Button.Disabled(model.Input = "")
                                   Button.OnClick
                                       (fun _ ->
                                           if model.Input <> "" then
                                               Submit |> dispatch) ] [
                            str "Guess"
                        ]
                    ]
                ]
                Column.column [] [
                    Columns.columns [ Columns.IsMultiline ] [
                        for pokemon in model.Pokemons ->
                            match pokemon with
                            | NotGuessedPokemon -> notGuessedPokemon
                            | GuessedPokemon poke -> showPokemon poke
                    ]
                ]
            ]
        ]
    ]
