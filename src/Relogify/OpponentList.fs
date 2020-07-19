module Relogify.OpponentList

open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.Graphql
open Xamarin.Forms

type Model =
    | Fetching of communities: Community list
    | FetchSuccess of playerNames: string list
    | FetchError of communities: Community list * errorMessage: string

type Msg =
    | FetchPlayers of Community list
    | PlayersFetched of string list
    | FetchPlayersError of communities: Community list * errorMessage: string
    | PlayerSelected of int option

type CmdMsg =
    | FetchPlayersCmdMsg of communities: Community list
    | DeselectPlayerCmdMsg

type OutMsg =
    | PlayerSelectedOutMsg of player: string

let initModel (communities : Community list): Model * CmdMsg list =
    match communities with
    | [] -> FetchError ([], "Application settings are empty"), []
    | _ -> Fetching communities, [FetchPlayersCmdMsg communities]

let fetchPlayersCmd (communities: Community list) =
    async {
        let communityNames = communities |> List.map (fun c -> c.CommunityName) |> Array.ofList
        let! result = getPlayersForCommunitiesOperation.AsyncRun(runtimeContext, communityNames)
        return
            match result.Data with
//            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> List.except [currentPlayerName] |> PlayersFetched
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> PlayersFetched
            | None -> FetchPlayersError (communities, sprintf "Error fetching players for communities '%A'. Please check your internet connection and try again. If the problem persists, check your settings and restart the app" communityNames)
    }
    |> Cmd.ofAsyncMsg

let listViewRef = ViewRef<CustomListView>()

let deselectPlayer () =
    listViewRef.TryValue |> Option.iter (fun listView -> listView.SelectedItem <- null)
    Cmd.none

let mapCommands: (CmdMsg -> Cmd<Msg>) =
    function
    | FetchPlayersCmdMsg communities -> fetchPlayersCmd communities
    | DeselectPlayerCmdMsg -> deselectPlayer ()

let update (model: Model) (msg: Msg) (currentPlayerName: string): Model * CmdMsg list * OutMsg option =
    match msg with
    | FetchPlayers communities -> Fetching communities, [FetchPlayersCmdMsg communities], None
    | PlayersFetched players -> FetchSuccess players, [], None
    | FetchPlayersError (communities, errorMessage) -> FetchError (communities, errorMessage), [], None
    | PlayerSelected maybeIndex ->
        match maybeIndex, model with
        | Some index, FetchSuccess players ->
            let selectedPlayer = players.[index]
            model, [DeselectPlayerCmdMsg], Some <| PlayerSelectedOutMsg selectedPlayer
        | _ -> model, [], None

let view (model: Model) (dispatch: Msg -> unit): ViewElement =
    View.ContentPage(
        title = "Players",
        icon = ImagePath "tab_feed.png",
        content =
            match model with
            | Fetching _ -> View.ActivityIndicator(isRunning = true)
            | FetchError (communities, errorMessage) ->
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
                            command = (fun _ -> dispatch <| FetchPlayers communities)
                        )
                    ]
                )
            | FetchSuccess players ->
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    children = [
                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            children = [
                                View.Button(text = "test")
                                View.Button(text = "test2")
                            ]
                        )
                        View.ListView(
                            ref = listViewRef,
                            isPullToRefreshEnabled = false,
                            items = (players |> List.map (fun playerName -> View.TextCell(text = playerName))),
                            itemSelected = (dispatch << PlayerSelected)
                        )
                    ]
                )
    )
