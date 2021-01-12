namespace Shared

open System

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName


type Pokemon =
    { Id: int
      Name: string
      ImageUrl: string }

module Pokemon =
    let isValid id name =
        match id, name with
        | x, y when x > 0 && String.IsNullOrWhiteSpace y |> not -> true
        | _ -> false

    let create id name url =
        { Id = id; Name = name; ImageUrl = url }

type IPokemonApi =
    { CheckPokemon: string -> Async<Pokemon> }
