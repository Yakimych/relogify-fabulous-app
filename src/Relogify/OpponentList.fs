module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql
open Xamarin.Forms

type Model =
    | NotAsked
    | Fetching of communityName: string
    | FetchSuccess of playerNames: string list
    | FetchError of errorMessage: string

type Msg =
    | PlayersFetched of string list
    | FetchPlayersError of string
    | PlayerSelected of int option

type CmdMsg =
    | FetchPlayersCmdMsg of communityName: string
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of player: string

// TODO: Is it ok for initModel to return a list of CmdMsgs and bypass update completely?
let initModel (applicationSettings: ApplicationSettings): Model * CmdMsg list =
    match applicationSettings.PlayerName, applicationSettings.CommunityName with
    | Some(_), Some(communityName) -> Fetching communityName, [FetchPlayersCmdMsg communityName]
    | _ -> NotAsked, []

let fetchPlayersCmd (communityName: string) =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, communityName)
        return
            match result.Data with
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> PlayersFetched
            | None -> FetchPlayersError <| sprintf "Error fetching players for community '%s'. Please check your internet connection and restart the app" communityName
    }
    |> Cmd.ofAsyncMsg

let listViewRef = ViewRef<CustomListView>()

let deselectPlayer () =
    listViewRef.TryValue |> Option.iter (fun listView -> listView.SelectedItem <- null)
    Cmd.none

let mapCommands =
    function
    | FetchPlayersCmdMsg communityName -> fetchPlayersCmd communityName
    | DeselectPlayerCmdMsg -> deselectPlayer ()

let update model msg: Model * CmdMsg list * OutMsg option =
    match msg with
    | PlayersFetched players -> FetchSuccess players, [], None
    | FetchPlayersError errorMessage -> FetchError errorMessage, [], None
    | PlayerSelected maybeIndex ->
        match maybeIndex, model with
        | Some index, FetchSuccess players ->
            let selectedPlayer = players.[index]
            model, [DeselectPlayerCmdMsg], Some <| PlayerSelectedOutMsg selectedPlayer
        | _ -> model, [], None

let view (model: Model) dispatch =
    View.ContentPage(
        title = "Play",
        icon = Image.Path "tab_feed.png",
        content =
            match model with
            | NotAsked -> View.Label()
            | Fetching _ -> View.ActivityIndicator(isRunning = true)
            | FetchError errorMessage ->
                View.Label(
                    text = errorMessage,
                    horizontalOptions = LayoutOptions.Center,
                    verticalOptions = LayoutOptions.Center,
                    margin = Thickness(20.0),
                    textColor = Color.Red
                )
                // TODO: Add a "Try again" button?
            | FetchSuccess players ->
                View.ListView(
                    ref = listViewRef,
                    isPullToRefreshEnabled = false,
                    items = (players |> List.map (fun playerName -> View.TextCell playerName)),
                    itemSelected = (fun index -> dispatch (PlayerSelected index))
                )
    )
