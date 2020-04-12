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
    | FetchPlayersCmdMsg of communityName: string
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of player: string

let initModel (applicationSettings: ApplicationSettings): Model * CmdMsg list =
    match applicationSettings.PlayerName, applicationSettings.CommunityName with
    | Some(_), Some(communityName) -> Fetching communityName, [FetchPlayersCmdMsg communityName]
    | _ -> FetchError ("", "Application settings are empty"), []

let fetchPlayersCmd (communityName: string) =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, communityName)
        return
            match result.Data with
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> PlayersFetched
            | None -> FetchPlayersError (communityName, sprintf "Error fetching players for community '%s'. Please check your internet connection and try again. If the problem persists, check your settings and restart the app" communityName)
    }
    |> Cmd.ofAsyncMsg

let listViewRef = ViewRef<CustomListView>()

let deselectPlayer () =
    listViewRef.TryValue |> Option.iter (fun listView -> listView.SelectedItem <- null)
    Cmd.none

let mapCommands: (CmdMsg -> Cmd<Msg>) =
    function
    | FetchPlayersCmdMsg communityName -> fetchPlayersCmd communityName
    | DeselectPlayerCmdMsg -> deselectPlayer ()

let update model msg: Model * CmdMsg list * OutMsg option =
    match msg with
    | FetchPlayers communityName -> Fetching communityName, [FetchPlayersCmdMsg communityName], None
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
                    items = (players |> List.map (fun playerName -> View.TextCell playerName)),
                    itemSelected = (fun index -> dispatch (PlayerSelected index))
                )
    )
