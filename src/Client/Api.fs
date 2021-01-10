module Api

open Fable.Remoting.Client
open Shared

let pokemonApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IPokemonApi>
