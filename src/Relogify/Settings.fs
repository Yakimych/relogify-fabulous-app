module Relogify.Settings

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open System
open Xamarin.Forms

type Settings = { CommunityName: string; PlayerName: string }

type Model =
    | Viewing of Settings
    | EditingCommunityName of Settings * string
    | FetchingPlayers of Settings * string
    | ChoosingPlayer of Settings * string list * Settings // CurrentSettings, Player list, and Settings under edit

type Msg =
    | OpenDialog
    | SetCommunityName of string
    | StartFetchingPlayers
    | FetchingPlayersSuccess of string list
    | BackToEditCommunity
    | SelectPlayer of string
    | SaveSettings
    | CancelDialog

let performTransition (state: Model) (transition: Msg): Model =
    match (state, transition) with
    | (Viewing currentSettings, OpenDialog) ->
        EditingCommunityName (currentSettings, currentSettings.CommunityName)
    | (EditingCommunityName (currentSettings, _), SetCommunityName newCommunityName) ->
        EditingCommunityName (currentSettings, newCommunityName)
    | (EditingCommunityName (currentSettings, communityName), StartFetchingPlayers) ->
        FetchingPlayers (currentSettings, communityName) // TODO: FetchPlayers Cmd
    | (FetchingPlayers (currentSettings, communityName), FetchingPlayersSuccess playerList) ->
        ChoosingPlayer (currentSettings, playerList, { CommunityName = communityName; PlayerName = "" })
//    | (FetchingPlayers, FetchingPlayersError errorMessage) ->
//        ErrorFetchingPlayers errorMessage
    | (ChoosingPlayer (currentSettings, playerList, newSettings), SelectPlayer newSelectedPlayer) ->
        ChoosingPlayer (currentSettings, playerList, { newSettings with PlayerName = newSelectedPlayer })
    | (ChoosingPlayer (currentSettings, playerList, newSettings), BackToEditCommunity) ->
        EditingCommunityName (currentSettings, newSettings.CommunityName)
    | (EditingCommunityName (currentSettings, _), CancelDialog) ->
        Viewing currentSettings
    | (ChoosingPlayer (currentSettings, _, _), CancelDialog) ->
        Viewing currentSettings
    | (ChoosingPlayer (_, playerList, newSettings), SaveSettings) ->
        Viewing newSettings // TODO: SaveSettings Cmd
    | (currentState, disallowedTransition) -> currentState

let initModel = Model.Viewing { CommunityName = "testcommunity"; PlayerName = "testplayer" }

type CmdMsg = FetchPlayersCmdMsg

let fetchPlayersCmd () =
    async {
        do! Async.Sleep 200
        return FetchingPlayersSuccess ["player1"; "player2"]
    }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | FetchPlayersCmdMsg -> fetchPlayersCmd()

let update model msg: Model * CmdMsg list =
    let newModel = performTransition model msg
    newModel, []

let dialogIsOpen =
    function
    | Viewing _ -> false
    | _ -> true

let getCurrentSettings =
    function
    | Viewing currentSettings -> currentSettings
    | EditingCommunityName (currentSettings, _) -> currentSettings
    | FetchingPlayers (currentSettings, _) -> currentSettings
    | ChoosingPlayer (currentSettings, _, _) -> currentSettings

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

let shouldShowDialog =
    function
    | Viewing _ -> false
    | _ -> true

let canTriggerLoadPlayers communityName = String.IsNullOrEmpty(communityName) |> not

let canCancelDialog =
    function
    | Viewing _ -> false
    | EditingCommunityName (currentSettings, _) -> String.IsNullOrEmpty(currentSettings.CommunityName) |> not
    | FetchingPlayers (currentSettings, _) -> String.IsNullOrEmpty(currentSettings.CommunityName) |> not
    | ChoosingPlayer (currentSettings, _, _) -> String.IsNullOrEmpty(currentSettings.CommunityName) |> not

let communityInput (communityName: string) (isFetchingPlayers: bool) dispatch =
     View.StackLayout(
         margin = Thickness(10.0),
         children = [
             View.Label(text = "Step 1: Enter the community name")
             View.Entry(text = communityName) // TODO: , keyboard = Entry.KeyboardProperty())
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
     ).Row(0)

let playerInput (model: Model) dispatch =
    View.StackLayout(
        margin = Thickness(10.0),
        orientation = StackOrientation.Vertical,
        children = [
            View.Label(text = "Step 2: Select yur player in ")
            View.Label(text = "TODO: Formatted text")

            View.Picker(
                title = "Select Player",
                items = ["asd"],
                selectedIndex = 0 // TODO: "Bind" to the model
            )
            View.Grid(
                coldefs = [ Star; Star ],
                children = [
                    View.Button(
                        text = "Back",
                        backgroundColor = Color.Red,
                        width = 50.0
                        // TODO: Command
                    ).Column(0)
                    View.Button(
                        text = "Save",
                        backgroundColor = Color.LightGreen
                        // TODO: Command
                    ).Column(1)
                ]
            )
        ]
    ).Row(0)

// TODO: Break up the model into parts and only pass the dialogModel here
let dialogBody (model: Model) dispatch =
    View.StackLayout(
        isVisible = shouldShowDialog model,
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
                         rowdefs = [ Star; Auto ],
                         children = [
                             yield
                                 match model with
//                                 | Viewing _ -> failwith "TODO: Should never happen"
                                 | Viewing _ -> communityInput "" false dispatch // TODO: Remove
                                 | EditingCommunityName (_, communityName) -> communityInput communityName false dispatch
                                 | FetchingPlayers (_, communityName) -> communityInput communityName true dispatch
                                 | ChoosingPlayer (_, players, newSettings) -> playerInput model dispatch

                             yield View.Button(
                                     text = "Cancel",
                                     isVisible = canCancelDialog model,
                                     backgroundColor = Color.LightGray,
                                     command = (fun _ -> dispatch CancelDialog)
                                 ).Row(1)
                         ]
                     )
                ]
            )
        ]
    )

let view (model: Model) dispatch =
    let currentSettings = model |> getCurrentSettings

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
                               View.Label(text = currentSettings.CommunityName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(0).Column(1)
                               View.Label(text = "Player: ", fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(0)
                               View.Label(text = currentSettings.PlayerName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(1)
                               View.Button(text = "Edit", backgroundColor = Color.Orange, textColor = Color.Black,
                                           fontSize = FontSize.Named(NamedSize.Large),
                                           margin = Thickness(15.0), height = 60.0, cornerRadius = 10, borderWidth = 2.0,
                                           command = (fun _ -> dispatch OpenDialog)).Row(2).Column(0)
                                   .ColumnSpan(2) ])

                    yield dialogBackdrop (model |> shouldShowDialog)
                    yield dialogBody model dispatch
                ]
            )
    )



