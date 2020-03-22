module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Xamarin.Forms
open Relogify.Graphql

type FetchPlayersState =
    | NotAsked
    | Fetching of communityName: string
    | Fetched of playerNames: string list

type Model =
     { FetchPlayersState: FetchPlayersState }

type Msg =
    | FetchPlayersClicked
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

let update model (communityName: string) msg: Model * CmdMsg list =
    match msg with
    | FetchPlayersClicked -> model, [FetchPlayersCmdMsg communityName]
    | PlayersFetched players -> { model with FetchPlayersState = Fetched players }, []
    | FetchPlayerError errorMessage -> model, [] // TODO: Handle errors correctly

let getBodyElements (model: Model): ViewElement list =
    match model.FetchPlayersState with
    | NotAsked -> [View.Label(text = "Not asked")]
    | Fetching communityName -> [View.Label(sprintf "Fetching players in community: %s" communityName)]
    | Fetched players ->
        players
         |> List.map (fun playerName ->
                        View.Label(
                            text = playerName,
                            horizontalOptions = LayoutOptions.Center,
                            width = 200.0,
                            horizontalTextAlignment = TextAlignment.Center))

let view (model: Model) (playerName: string) dispatch =
    View.ContentPage(
        title = "Select Opponent",
        content = View.CollectionView(
            items = [
                View.StackLayout(
                     padding = Thickness 20.0,
                     verticalOptions = LayoutOptions.Center,
                     children =
                         [
                             yield View.Label(
                                text = playerName,
                                horizontalOptions = LayoutOptions.Center,
                                width = 200.0,
                                horizontalTextAlignment = TextAlignment.Center)

                             yield! getBodyElements model

//                                 yield! model.OpponentNames
//                                        |> Array.map (fun opponentName ->
//                                            View.Label(
//                                                   text = opponentName,
//                                                   horizontalOptions = LayoutOptions.Center,
//                                                   width = 200.0,
//                                                   horizontalTextAlignment = TextAlignment.Center)
//                                            )
                         ])
                ]
            )
        )
