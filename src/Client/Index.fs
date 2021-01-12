module Index

open Elmish
open Shared
open Api


type GameState =
    | StartScreen
    | Playing of Playing.Model
    | EndGame of EndGame.Model

type Model = { GameState: GameState }



type Msg =
    | PlayingMsg of Playing.Msg
    | StartMsg of Start.Msg
    | GameOverMsg of EndGame.Msg



let init (): Model * Cmd<Msg> =
    let model = { GameState = StartScreen }

    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match (msg, model.GameState) with
    | StartMsg _, StartScreen ->
        let nextState, nextCmd = Playing.init ()
        { GameState = Playing nextState }, nextCmd

    | PlayingMsg playingMsg, Playing game ->
        match game.Lives with
        | 1 ->
            let endModel = EndGame.init game.Score

            { model with
                  GameState = EndGame endModel },
            Cmd.none
        | _ ->
            let playingModel, cmd = Playing.update playingMsg game

            { model with
                  GameState = Playing playingModel },
            Cmd.map PlayingMsg cmd

    | GameOverMsg gameOverMsg, EndGame score ->
        match gameOverMsg with
        | EndGame.Restart ->
            let nextState, nextCmd = Playing.init ()
            { GameState = Playing nextState }, nextCmd

    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma


let view (model: Model) (dispatch: Msg -> unit) =

    match model.GameState with
    | StartScreen -> Start.view (StartMsg >> dispatch)

    | Playing game -> Playing.view game (PlayingMsg >> dispatch)

    | EndGame score -> EndGame.view score (GameOverMsg >> dispatch)
