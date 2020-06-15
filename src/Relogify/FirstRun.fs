module Relogify.FirstRun

open Fabulous
open Fabulous.XamarinForms
open Relogify
open Relogify.Graphql
open Relogify.ApplicationSettings
open Xamarin.Forms
open System

// TODO: Remove duplication with Settings.fs

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
    | SaveFirstRunInitiated

type CmdMsg =
    | FetchPlayersCmdMsg of communityName: string

type OutMsg =
    | FirstRunComplete of communityName: string * playerName : string

let performTransition (state: Model) (transition: Msg): Model * CmdMsg list * OutMsg option =
    match (state, transition) with
    | (EditingCommunityName _, SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, [], None
    | (EditingCommunityName communityName, StartFetchingPlayers) when communityName |> isNotEmpty ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName], None

    | (FetchingPlayers communityName, FetchingPlayersSuccess playerList) ->
        ChoosingPlayer (playerList, { CommunityName = communityName; PlayerName = "" }), [], None
    | (FetchingPlayers communityName, FetchingPlayersError errorMessage) ->
        FailedFetchingPlayers (communityName, errorMessage), [], None

    | (FailedFetchingPlayers (_, _), SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, [], None
    | (FailedFetchingPlayers (communityName, _), StartFetchingPlayers) ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName], None

    | (ChoosingPlayer (_, newSettings), BackToEditCommunity) ->
        EditingCommunityName (newSettings.CommunityName), [], None
    | (ChoosingPlayer (playerList, newSettings), SelectPlayer newSelectedPlayer) ->
        ChoosingPlayer (playerList, { newSettings with PlayerName = newSelectedPlayer }), [], None

    | (ChoosingPlayer (players, communityUnderEdit), SaveFirstRunInitiated) ->
        // TODO: SavingInitialSettings? Spinner before the page gets closed?
        ChoosingPlayer (players, communityUnderEdit), [], Some (FirstRunComplete (communityUnderEdit.CommunityName, communityUnderEdit.PlayerName))

    | (currentState, _disallowedTransition) -> currentState, [], None

let initModel (communityNameHasBeenSaved: bool) =
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

let mapCommands =
    function
    | FetchPlayersCmdMsg communityName -> fetchPlayersCmd communityName

let update (model: Model) (msg: Msg): Model * CmdMsg list * OutMsg option =
    // TODO: Move the code from performTransition here
    let newModel, cmdMsgList, maybeOutMsg = performTransition model msg
    newModel, cmdMsgList, maybeOutMsg

let isFetchingPlayers =
    function
    | FetchingPlayers _ -> true
    | _ -> false

let dialogBackdrop (isVisible: bool): ViewElement =
    View.StackLayout(
        isVisible = isVisible,
        backgroundColor = Color.Black,
        orientation = StackOrientation.Vertical,
        verticalOptions = LayoutOptions.FillAndExpand,
        opacity = 0.5,
        children = [
            View.Label(text = "asd", verticalOptions = LayoutOptions.FillAndExpand)
        ]
    )

let communityInput (communityName: string) (isFetchingPlayers: bool) dispatch =
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

let dialogBody (model: Model) (savedCommunityName: string) dispatch =
    View.StackLayout(
        margin = Thickness(30.0, 50.0, 30.0, 50.0),
        backgroundColor = Color.White,
        orientation = StackOrientation.Vertical,
        horizontalOptions = LayoutOptions.FillAndExpand,
        verticalOptions = LayoutOptions.FillAndExpand,
        children = [
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
                             yield!
                                 match model with
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
                                                     command = (fun _ -> dispatch SaveFirstRunInitiated),
                                                     commandCanExecute = (not <| String.IsNullOrEmpty(newSettings.PlayerName))
                                                 ).Column(1)
                                             ]
                                         ).Row(1)
                                     ]
                         ]
                     )
                ]
            )
        ]
    )

let view (model: Model) (communities: Community list) dispatch =
    View.ContentPage
        (title = "Select player",
         content = View.Grid (children = [ dialogBody model "" dispatch ]))
