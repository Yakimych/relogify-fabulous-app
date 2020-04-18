module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql
open Xamarin.Forms

type Model =
    | Fetching of communityName: string
    | FetchSuccess of playerNames: string list
    | FetchError of communityName: string * errorMessage: string

type Msg =
    | FetchPlayers of string
    | PlayersFetched of string list
    | FetchPlayersError of communityName: string * errorMessage: string
    | PlayerSelected of int option

type CmdMsg =
    | FetchPlayersCmdMsg of communityName: string * currentPlayerName: string
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of player: string

let initModel (community : Community option): Model * CmdMsg list =
    match community with
    | Some({CommunityName = communityName; PlayerName = currentPlayerName}) -> Fetching communityName, [FetchPlayersCmdMsg (communityName, currentPlayerName)]
    | _ -> FetchError ("", "Application settings are empty"), []

let fetchPlayersCmd (communityName: string) (currentPlayerName: string) =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, communityName)
        return
            match result.Data with
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> List.except [currentPlayerName] |> PlayersFetched
            | None -> FetchPlayersError (communityName, sprintf "Error fetching players for community '%s'. Please check your internet connection and try again. If the problem persists, check your settings and restart the app" communityName)
    }
    |> Cmd.ofAsyncMsg

let listViewRef = ViewRef<CustomListView>()

let deselectPlayer () =
    listViewRef.TryValue |> Option.iter (fun listView -> listView.SelectedItem <- null)
    Cmd.none

let mapCommands: (CmdMsg -> Cmd<Msg>) =
    function
    | FetchPlayersCmdMsg (communityName, currentPlayerName) -> fetchPlayersCmd communityName currentPlayerName
    | DeselectPlayerCmdMsg -> deselectPlayer ()

let update (model: Model) (msg: Msg) (currentPlayerName: string): Model * CmdMsg list * OutMsg option =
    match msg with
    | FetchPlayers communityName -> Fetching communityName, [FetchPlayersCmdMsg (communityName, currentPlayerName)], None
    | PlayersFetched players -> FetchSuccess players, [], None
    | FetchPlayersError (communityName, errorMessage) -> FetchError (communityName, errorMessage), [], None
    | PlayerSelected maybeIndex ->
        match maybeIndex, model with
        | Some index, FetchSuccess players ->
            let selectedPlayer = players.[index]
            model, [DeselectPlayerCmdMsg], Some <| PlayerSelectedOutMsg selectedPlayer
        | _ -> model, [], None

let view (model: Model) (dispatch: Msg -> unit): ViewElement =
    View.ContentPage(
        title = "Players",
        icon = Image.Path "tab_feed.png",
        content =
            match model with
            | Fetching _ -> View.ActivityIndicator(isRunning = true)
            | FetchError (communityName, errorMessage) ->
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    children = [
                        View.Label(
                            text = errorMessage,
                            horizontalOptions = LayoutOptions.Center,
                            verticalOptions = LayoutOptions.Center,
                            margin = Thickness(20.0),
                            textColor = Color.Red
                        )
                        View.Button(
                            text = "Try again",
                            command = (fun _ -> dispatch <| FetchPlayers communityName)
                        )
                    ]
                )
            | FetchSuccess players ->
                View.ListView(
                    ref = listViewRef,
                    isPullToRefreshEnabled = false,
                    items = (players |> List.map (fun playerName -> View.TextCell(text = playerName))),
                    itemSelected = (dispatch << PlayerSelected)
                )
    )
