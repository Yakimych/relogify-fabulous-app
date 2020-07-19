module Relogify.PlayerSelector

open Fabulous
open Fabulous.XamarinForms
open Relogify
open Relogify.Graphql
open Relogify.ApplicationSettings
open Xamarin.Forms
open System

type Model =
    | EditingCommunityName of communityName: string
    | FetchingPlayers of communityName: string
    | FailedFetchingPlayers of communityName: string * errorMessage: string
    | ChoosingPlayer of players: string list * communityUnderEdit: Community

type Msg =
    | SetCommunityName of newCommunityName: string
    | StartFetchingPlayers
    | FetchingPlayersSuccess of fetchedPlayers: string list
    | FetchingPlayersError of errorMessage: string
    | BackToEditCommunity
    | SelectPlayer of selectedPlayer: string
    | Confirm of communityName: string * playerName: string
    | Abort

type CmdMsg =
    | FetchPlayersCmdMsg of communityName: string

let performTransition (state: Model) (transition: Msg): Model * CmdMsg list =
    match (state, transition) with
    | (EditingCommunityName _, SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, []
    | (EditingCommunityName communityName, StartFetchingPlayers) when communityName |> isNotEmpty ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName]

    | (FetchingPlayers communityName, FetchingPlayersSuccess playerList) ->
        ChoosingPlayer (playerList, { CommunityName = communityName; PlayerName = "" }), []
    | (FetchingPlayers communityName, FetchingPlayersError errorMessage) ->
        FailedFetchingPlayers (communityName, errorMessage), []

    | (FailedFetchingPlayers (_, _), SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, []
    | (FailedFetchingPlayers (communityName, _), StartFetchingPlayers) ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName]

    | (ChoosingPlayer (_, newSettings), BackToEditCommunity) ->
        EditingCommunityName (newSettings.CommunityName), []
    | (ChoosingPlayer (playerList, newSettings), SelectPlayer newSelectedPlayer) ->
        ChoosingPlayer (playerList, { newSettings with PlayerName = newSelectedPlayer }), []

    | (currentState, _disallowedTransition) -> currentState, []

let initModel (): Model =
    EditingCommunityName ""

let fetchPlayersCmd (communityName: string) =
    async {
        let! result = getPlayersOperation.AsyncRun(runtimeContext, communityName)
        return
            match result.Data with
            | Some data -> data.Players |> List.ofArray |> List.map (fun p -> p.Name) |> FetchingPlayersSuccess
            | None -> FetchingPlayersError (sprintf "Failed to fetch players for community '%s'. Please check your internet connection and try again." communityName)
    }
    |> Cmd.ofAsyncMsg

let mapCommands (cmdMsg: CmdMsg): Cmd<Msg> =
    match cmdMsg with
    | FetchPlayersCmdMsg communityName -> fetchPlayersCmd communityName

let communityInput (communityName: string) (isFetchingPlayers: bool) (dispatch: Msg -> unit): ViewElement =
     View.StackLayout(
         margin = Thickness(10.0),
         children = [
             View.Label(text = "Step 1: Enter the community name")
             View.Entry(
                text = communityName,
                keyboard = Keyboard.Plain,
                textChanged = (fun args -> dispatch (SetCommunityName args.NewTextValue))
             )
             View.Label(text = "The community name is the last part of your Relogify URL: ")
             View.Label(
                 formattedText = View.FormattedString(
                    spans = [
                        View.Span(text = "https://relogify.com/")
                        View.Span(text = "communityname", fontAttributes = FontAttributes.Bold)
                     ]
                 )
             )
             View.Grid(children = [
                 View.Button(
                     text = "Fetch Players",
                     backgroundColor = Color.LightGreen,
                     command = (fun _ -> dispatch StartFetchingPlayers),
                     commandCanExecute = isNotEmpty communityName
                 )
                 View.ActivityIndicator(
                    horizontalOptions = LayoutOptions.End,
                    margin = Thickness(0.0, 0.0, 40.0, 0.0),
                    isVisible = isFetchingPlayers,
                    isRunning = isFetchingPlayers
                 )
             ])
         ]
     )

let playerInput (players: string list) (communityName: string) dispatch =
    let getPlayerByMaybeIndex (maybeIndex: int option) (players: string list) =
        maybeIndex
        |> Option.bind (fun index -> players |> List.tryItem(index))
        |> Option.defaultValue ""

    View.StackLayout(
        margin = Thickness(10.0),
        orientation = StackOrientation.Vertical,
        children = [
            View.Label(
                formattedText =
                    View.FormattedString(
                        spans = [
                            View.Span(text = "Step 2: Select your player in ")
                            View.Span(text = communityName, fontAttributes = FontAttributes.Bold)
                        ]
                    )
            )
            View.ListView(
                items = (players |> List.map (fun player -> View.TextCell player)),
                itemSelected = (fun maybeIndex -> dispatch <| SelectPlayer (players |> getPlayerByMaybeIndex maybeIndex))
            )

        ]
    )

let playerSelectorView (dialogState: Model) (dispatch: Msg -> unit): ViewElement list =
    match dialogState with
    | EditingCommunityName communityName -> [(communityInput communityName false dispatch).Row(0)]
    | FetchingPlayers communityName -> [(communityInput communityName true dispatch).Row(0)]
    | FailedFetchingPlayers (communityName, errorMessage) ->
        [
            View.StackLayout(
                children = [
                    communityInput communityName false dispatch
                    View.Label(text = errorMessage, textColor = Color.Red)
                ]
            ).Row(0)
        ]
    | ChoosingPlayer (players, newSettings) ->
        [
            (playerInput players newSettings.CommunityName dispatch).Row(0)

            View.Grid(
             coldefs = [ Star; Star ],
             children = [
                 View.Button(
                     text = "Back",
                     backgroundColor = Color.Red,
                     command = (fun _ -> dispatch BackToEditCommunity)
                 ).Column(0)

                 View.Button(
                     text = "Save",
                     backgroundColor = Color.LightGreen,
                     command = (fun _ -> dispatch <| Confirm (newSettings.CommunityName, newSettings.PlayerName)),
                     commandCanExecute = (not <| String.IsNullOrEmpty(newSettings.PlayerName))
                 ).Column(1)
             ]
            ).Row(1)
        ]

let view (model: Model) (dispatch: Msg -> unit): ViewElement =
    View.StackLayout(
        orientation = StackOrientation.Vertical,
        horizontalOptions = LayoutOptions.FillAndExpand,
        verticalOptions = LayoutOptions.FillAndExpand,
        children = [
            View.StackLayout(
                height = 60.0,
                backgroundColor = Color.Accent,
                horizontalOptions = LayoutOptions.FillAndExpand,
                orientation = StackOrientation.Horizontal,
                children = [
                    View.Label(
                          text = "Select Player",
                          textColor = Color.White,
                          horizontalOptions = LayoutOptions.FillAndExpand,
                          verticalOptions = LayoutOptions.FillAndExpand,
                          fontSize = FontSize.Named(NamedSize.Large),
                          margin = Thickness(20.0, 10.0, 0.0, 10.0)
                      )
                ]
            )

            View.Grid(
                 margin = Thickness(10.0),
                 verticalOptions = LayoutOptions.FillAndExpand,
                 rowdefs = [ Star; Absolute 50.0; Auto ],
                 children = [
                     yield! playerSelectorView model dispatch

                     yield View.Button(
                             text = "Cancel",
                             backgroundColor = Color.LightGray,
                             command = (fun _ -> dispatch Abort)
                         ).Row(2)
                 ]
             )
        ]
    )
