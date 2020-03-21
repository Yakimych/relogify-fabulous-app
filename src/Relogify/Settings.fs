module Relogify.Settings

open Fabulous
open Fabulous.XamarinForms
open Relogify
open Relogify.ApplicationSettings
open Xamarin.Forms
open System

type Settings = { CommunityName: string; PlayerName: string }

type DialogState =
    | Closed
    | EditingCommunityName of communityName: string
    | FetchingPlayers of communityName: string
    | FailedFetchingPlayers of communityName : string * errorMessage: string
    | ChoosingPlayer of players: string list * settingsUnderEdit: Settings

type Model = { Settings: Settings; DialogState: DialogState }

type Msg =
    | OpenDialog of savedCommunityName: string
    | SetCommunityName of newCommunityName: string
    | StartFetchingPlayers
    | FetchingPlayersSuccess of fetchedPlayers: string list
    | FetchingPlayersError of errorMessage: string
    | BackToEditCommunity
    | SelectPlayer of selectedPlayer: string
    | SaveSettings
    | SettingsSaved of Settings
    | CancelDialog

type CmdMsg =
    | FetchPlayersCmdMsg
    | SaveSettingsCmdMsg of Settings

let toApplicationSettings (settings: Settings): ApplicationSettings =
    { CommunityName = settings.CommunityName |> stringToOption
      PlayerName = settings.PlayerName |> stringToOption }

let performTransition (state: DialogState) (transition: Msg): DialogState * CmdMsg list =
    match (state, transition) with
    | (Closed, OpenDialog savedCommunityName) ->
        EditingCommunityName savedCommunityName, []

    | (EditingCommunityName _, SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, []
    | (EditingCommunityName communityName, StartFetchingPlayers) ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg]
    | (EditingCommunityName (_), CancelDialog) ->
        Closed, []

    | (FetchingPlayers communityName, FetchingPlayersSuccess playerList) ->
        ChoosingPlayer (playerList, { CommunityName = communityName; PlayerName = "" }), []
    | (FetchingPlayers communityName, FetchingPlayersError errorMessage) ->
        FailedFetchingPlayers (communityName, errorMessage), []

    | (FailedFetchingPlayers (_, _), SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, []
    | (FailedFetchingPlayers (communityName, _), StartFetchingPlayers) ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg]
    | (FailedFetchingPlayers (_, _), CancelDialog) ->
        Closed, []

    | (ChoosingPlayer (_, newSettings), BackToEditCommunity) ->
        EditingCommunityName (newSettings.CommunityName), []
    | (ChoosingPlayer (playerList, newSettings), SelectPlayer newSelectedPlayer) ->
        ChoosingPlayer (playerList, { newSettings with PlayerName = newSelectedPlayer }), []
    | (ChoosingPlayer (_, _), CancelDialog) ->
        Closed, []
    | (ChoosingPlayer (_, settingsUnderEdit), SaveSettings) ->
        Closed, [SaveSettingsCmdMsg settingsUnderEdit]

    | (currentState, _disallowedTransition) -> currentState, []

let initModel (settings: ApplicationSettings) =
    let communityName = settings.CommunityName |> Option.defaultValue ""
    let playerName = settings.PlayerName |> Option.defaultValue ""
    { Settings = { CommunityName = communityName; PlayerName = playerName }; DialogState = Closed }

let rand = Random()
let fetchPlayersCmd () =
    async {
        do! Async.Sleep 200

        if rand.Next(2) = 0 then
            return FetchingPlayersError "Failed to fetch, please check your internet connection and try again"
        else
            return FetchingPlayersSuccess ([1 .. 20] |> List.map (sprintf "player%d"))
    }
    |> Cmd.ofAsyncMsg

let saveSettingsCmd (settings: Settings) =
    async {
        do! ApplicationSettings.saveApplicationSettings settings.CommunityName settings.PlayerName |> Async.AwaitTask
        return SettingsSaved settings
    }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | FetchPlayersCmdMsg -> fetchPlayersCmd()
    | SaveSettingsCmdMsg settings -> saveSettingsCmd settings

let update model msg: Model * CmdMsg list =
    let newDialogState, cmdMsgList = performTransition model.DialogState msg
    { model with DialogState = newDialogState }, cmdMsgList

let dialogIsOpen =
    function
    | Closed _ -> false
    | _ -> true

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

let canTriggerLoadPlayers communityName = not <| String.IsNullOrEmpty(communityName)

let canCancelDialog (model: Model) =
    match model.DialogState with
    | Closed _ -> false
    | _ -> not <| String.IsNullOrWhiteSpace(model.Settings.CommunityName)

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
             View.Label(text = "TODO: Formatted text")
             View.Grid(children = [
                 View.Button(
                     text = "Fetch Players",
                     isEnabled = canTriggerLoadPlayers communityName,
                     backgroundColor = Color.LightGreen,
                     command = (fun _ -> dispatch StartFetchingPlayers)
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

let playerInput (players: string list) dispatch =
    let getPlayerByMaybeIndex (maybeIndex: int option) (players: string list) =
        maybeIndex
        |> Option.bind (fun index -> players |> List.tryItem(index))
        |> Option.defaultValue ""

    View.StackLayout(
        margin = Thickness(10.0),
        orientation = StackOrientation.Vertical,
        children = [
            View.Label(text = "Step 2: Select your player in ")
            View.Label(text = "TODO: Formatted text")

            View.ListView(
                items = (players |> List.map (fun player -> View.TextCell player)),
                itemSelected = (fun maybeIndex -> dispatch <| SelectPlayer (players |> getPlayerByMaybeIndex maybeIndex))
            )

        ]
    )

// TODO: Break up the model into parts and only pass the dialogModel here
let dialogBody (model: Model) dispatch =
    View.StackLayout(
        isVisible = dialogIsOpen model.DialogState,
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
                                 match model.DialogState with
//                                 | Closed _ -> failwith "TODO: Should never happen"
                                 | Closed _ -> [communityInput "" false dispatch] // TODO: Remove?
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
                                         (playerInput players dispatch).Row(0)

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
                                                     command = (fun _ -> dispatch SaveSettings),
                                                     commandCanExecute = (not <| String.IsNullOrEmpty(newSettings.PlayerName))
                                                 ).Column(1)
                                             ]
                                         ).Row(1)
                                     ]

                             yield View.Button(
                                     text = "Cancel",
                                     isVisible = canCancelDialog model,
                                     backgroundColor = Color.LightGray,
                                     command = (fun _ -> dispatch CancelDialog)
                                 ).Row(2)
                         ]
                     )
                ]
            )
        ]
    )

let view (model: Model) dispatch =
    View.ContentPage(
        title = "Settings",
        content =
            View.Grid(
                children = [
                    yield View.Grid
                        (margin = Thickness(10.0),
                         coldefs = [ Auto; Star ],
                         rowdefs = [ for _ in 1 .. 3 -> Auto ],
                         children =
                             [ View.Label(text = "Community: ", fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(0).Column(0)
                               View.Label(text = model.Settings.CommunityName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(0).Column(1)
                               View.Label(text = "Player: ", fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(0)
                               View.Label(text = model.Settings.PlayerName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(1)
                               View.Button(text = "Edit", backgroundColor = Color.Orange, textColor = Color.Black,
                                           fontSize = FontSize.Named(NamedSize.Large),
                                           margin = Thickness(15.0), height = 60.0, cornerRadius = 10, borderWidth = 2.0,
                                           command = (fun _ -> dispatch (OpenDialog model.Settings.CommunityName))).Row(2).Column(0)
                                   .ColumnSpan(2) ])

                    yield dialogBackdrop (model.DialogState |> dialogIsOpen)
                    yield dialogBody model dispatch
                ]
            )
    )

