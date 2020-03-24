module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql

type FetchPlayersState =
    | NotAsked
    | Fetching of communityName: string
    | Fetched of playerNames: string list

// TODO: Do we need a record here?
type Model =
     { FetchPlayersState: FetchPlayersState }

type Msg =
    | PlayersFetched of string list
    | FetchPlayerError of string

type CmdMsg = FetchPlayersCmdMsg of communityName: string

let initModel (applicationSettings: ApplicationSettings): Model * CmdMsg list =
    let cmdMsgs =
        match applicationSettings.PlayerName, applicationSettings.CommunityName with
        | Some(_), Some(communityName) -> [FetchPlayersCmdMsg communityName]
        | _ -> []

    { FetchPlayersState = NotAsked }, cmdMsgs

let fetchPlayersCmd (communityName: string) =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, communityName)
        return
            match result.Data with
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> PlayersFetched
            | None -> FetchPlayerError "Error fetching players" // TODO: Handle errors correctly
    }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | FetchPlayersCmdMsg communityName -> fetchPlayersCmd communityName

let update model msg: Model * CmdMsg list =
    match msg with
    | PlayersFetched players -> { model with FetchPlayersState = Fetched players }, []
    | FetchPlayerError errorMessage -> model, [] // TODO: Handle errors correctly

let view (model: Model) (playerName: string) (communityName: string) dispatch =
    View.ContentPage(
        title = sprintf "%s: select opponent in %s" playerName communityName,
        content =
            match model.FetchPlayersState with
            | NotAsked -> View.Label(text = "Not asked")
            | Fetching communityName -> View.Label(sprintf "Fetching players in community: %s" communityName)
            | Fetched players ->
                View.ListView(
                    items = (players |> List.map (fun playerName -> View.TextCell playerName))
                )
    )
