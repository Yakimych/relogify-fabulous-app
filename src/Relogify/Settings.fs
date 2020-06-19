module Relogify.Settings

open Fabulous
open Fabulous.XamarinForms
open Relogify
open Relogify.Graphql
open Relogify.ApplicationSettings
open Xamarin.Forms
open System

type DialogState =
    | Closed
    | EditingCommunityName of communityName: string
    | FetchingPlayers of communityName: string
    | FailedFetchingPlayers of communityName: string * errorMessage: string
    | ChoosingPlayer of players: string list * communityUnderEdit: Community

type Model =
    { DialogState: DialogState }

type OutMsg =
    | SettingsUpdated of ApplicationSettings
    | CommunitySelected of community: Community 

type Msg =
    | OpenDialog of savedCommunityName: string
    | SetCommunityName of newCommunityName: string
    | StartFetchingPlayers
    | FetchingPlayersSuccess of fetchedPlayers: string list
    | FetchingPlayersError of errorMessage: string
    | BackToEditCommunity
    | SelectPlayer of selectedPlayer: string
    | CommunityOnSave
    | CommunityOnDelete of communityName : string
    | CommunityOnSelect of community: Community // TODO: remove when tabs with communities are implemented
    | SettingsSaved of ApplicationSettings
    | CancelDialog

type CmdMsg =
    | FetchPlayersCmdMsg of communityName: string

let communitiesMatch (community : Community) (communityToCheck : Community) = 
    communityToCheck.CommunityName = community.CommunityName

let canDelete (communities : Community list) =
    communities |> List.length > 1

let updateCommunity (communityToSave : Community) (communities : Community list) : Community list =
    match communities |> List.exists (communitiesMatch communityToSave) with
    | false -> communities @ [communityToSave]
    | true -> communities |> List.map (fun community ->
        if communitiesMatch community communityToSave then communityToSave else community
    )

let deleteCommunity (communityNameToDelete : string) (communities : Community list) =
    if canDelete communities then
        communities |> List.filter (fun community -> community.CommunityName <> communityNameToDelete)
    else 
        communities

let deleteCommunityFromSettings (currentSettings: ApplicationSettings) (communityName : string) =
    { currentSettings with Communities = currentSettings.Communities |> deleteCommunity communityName }

let addOrUpdateCommunityFromSettings (currentSettings: ApplicationSettings) (community: Community) =
    { currentSettings with Communities = currentSettings.Communities |> updateCommunity community }

let performTransition (state: DialogState) (currentSettings: ApplicationSettings) (transition: Msg): DialogState * CmdMsg list * OutMsg option =
    match (state, transition) with
    | (Closed, OpenDialog savedCommunityName) ->
        EditingCommunityName savedCommunityName, [], None

    | (EditingCommunityName _, SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, [], None
    | (EditingCommunityName communityName, StartFetchingPlayers) when communityName |> isNotEmpty ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName], None
    | (EditingCommunityName (_), CancelDialog) ->
        Closed, [], None

    | (FetchingPlayers communityName, FetchingPlayersSuccess playerList) ->
        ChoosingPlayer (playerList, { CommunityName = communityName; PlayerName = "" }), [], None
    | (FetchingPlayers communityName, FetchingPlayersError errorMessage) ->
        FailedFetchingPlayers (communityName, errorMessage), [], None

    | (FailedFetchingPlayers (_, _), SetCommunityName newCommunityName) ->
        EditingCommunityName newCommunityName, [], None
    | (FailedFetchingPlayers (communityName, _), StartFetchingPlayers) ->
        FetchingPlayers communityName, [FetchPlayersCmdMsg communityName], None
    | (FailedFetchingPlayers (_, _), CancelDialog) ->
        Closed, [], None

    | (Closed, CommunityOnSelect community) ->
        Closed, [], CommunitySelected community |> Some

    | (ChoosingPlayer (_, newSettings), BackToEditCommunity) ->
        EditingCommunityName (newSettings.CommunityName), [], None
    | (ChoosingPlayer (playerList, newSettings), SelectPlayer newSelectedPlayer) ->
        ChoosingPlayer (playerList, { newSettings with PlayerName = newSelectedPlayer }), [], None
    | (ChoosingPlayer (_, _), CancelDialog) ->
        Closed, [], None
    | (ChoosingPlayer (_, communityUnderEdit), CommunityOnSave) ->
        Closed, [], addOrUpdateCommunityFromSettings currentSettings communityUnderEdit|> SettingsUpdated |> Some

    | (Closed, CommunityOnDelete communityNameToDelete) ->
        Closed, [], deleteCommunityFromSettings currentSettings communityNameToDelete |> SettingsUpdated |> Some

    | (currentState, _disallowedTransition) -> currentState, [], None

let initModel (communityNameHasBeenSaved: bool) =
    if communityNameHasBeenSaved then
        { DialogState = Closed }
    else
        { DialogState = EditingCommunityName "" }

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

let update (model: Model) (currentSettings: ApplicationSettings) (msg: Msg): Model * CmdMsg list * OutMsg option =
    let newDialogState, cmdMsgList, outMsg = performTransition model.DialogState currentSettings msg
    { model with DialogState = newDialogState }, cmdMsgList, outMsg

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

// TODO: Break up the model into parts and only pass the dialogModel here
let dialogBody (model: Model) (savedCommunityName: string) dispatch =
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
                                                     command = (fun _ -> dispatch CommunityOnSave),
                                                     commandCanExecute = (not <| String.IsNullOrEmpty(newSettings.PlayerName))
                                                 ).Column(1)
                                             ]
                                         ).Row(1)
                                     ]

                             yield View.Button(
                                     text = "Cancel",
                                     backgroundColor = Color.LightGray,
                                     command = (fun _ -> dispatch CancelDialog)
                                 ).Row(2)
                         ]
                     )
                ]
            )
        ]
    )

let viewCommunityListItem (isDeleteVisible : bool) dispatch (community: Community) =
    View.ViewCell
        (view =
            View.Grid(
                children = [
                    yield View.Grid
                        (margin = Thickness(10.0),
                         coldefs = [ Auto; Star ],
                         rowdefs = [ for _ in 1 .. 3 -> Auto ],
                         children =
                             [ View.Label(text = "Community: ", fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(0).Column(0)
                               View.Label(text = community.CommunityName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(0).Column(1)
                               View.Label(text = "Player: ", fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(0)
                               View.Label(text = community.PlayerName, fontSize = FontSize.Named(NamedSize.Large),
                                          margin = Thickness(left = 15.0, top = 10.0, right = 0.0, bottom = 0.0)).Row(1).Column(1)
                               ])
                    ]
               ),
        contextActions = [
                View.MenuItem(
                    text = "Delete", 
                    command = (fun _ -> dispatch (CommunityOnDelete community.CommunityName)), 
                    commandCanExecute = isDeleteVisible
                )]
        )


let view (model: Model) (communities: Community list) dispatch =
    View.ContentPage
        (title = "Settings", icon = Image.Path "tab_settings.png",
         content =
             View.Grid
                 (children =
                     [ View.AbsoluteLayout
                         (children =
                             [ View.ListView(
                                items = (communities |> List.map (viewCommunityListItem (canDelete communities) dispatch)),
                                itemTapped = (fun index -> communities |> List.item index |> CommunityOnSelect |> dispatch),
                                hasUnevenRows = true
                               )
                               View.Button(
                                    text = "Add", 
                                    backgroundColor = Color.Orange, 
                                    textColor = Color.Black,
                                    fontSize = FontSize.Named(NamedSize.Large), 
                                    margin = Thickness(15.0),
                                    height = 60.0, 
                                    cornerRadius = 10,
                                    borderWidth = 2.0,
                                    command = (fun _ -> dispatch (OpenDialog ""))
                                )
                                   .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                                   .LayoutBounds(Rectangle(1.0, 1.0, 80.0, 80.0)) ])
                       dialogBackdrop (model.DialogState |> dialogIsOpen)
                       dialogBody model "" dispatch ]))
                       