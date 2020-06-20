module Relogify.Settings

open Fabulous
open Fabulous.XamarinForms
open Relogify
open Relogify.ApplicationSettings
open Xamarin.Forms

type Model =
    | DialogClosed
    | AddingCommunity of PlayerSelector.Model

type OutMsg =
    | SettingsUpdated of ApplicationSettings
    | CommunitySelected of community: Community 

type Msg =
    | OpenDialog of savedCommunityName: string
    | PlayerSelectorMsg of PlayerSelector.Msg
    | CommunityOnSave
    | CommunityOnDelete of communityName : string
    | CommunityOnSelect of community: Community // TODO: remove when tabs with communities are implemented
    | SettingsSaved of ApplicationSettings
    | CancelDialog

type CmdMsg =
    | PlayerSelectorCmdMsg of PlayerSelector.CmdMsg

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

let initModel (communityNameHasBeenSaved: bool) =
    if communityNameHasBeenSaved then
        DialogClosed
    else
        AddingCommunity <| PlayerSelector.initModel ()

let mapCommands (cmdMsg: CmdMsg): Cmd<Msg> =
    match cmdMsg with
    | PlayerSelectorCmdMsg playerSelectorCmdMsg -> PlayerSelector.mapCommands playerSelectorCmdMsg |> Cmd.map PlayerSelectorMsg

let update (model: Model) (currentSettings: ApplicationSettings) (msg: Msg): Model * CmdMsg list * OutMsg option =
    match (model, msg) with
    | (DialogClosed, OpenDialog communityName) ->
        AddingCommunity (PlayerSelector.EditingCommunityName communityName), [], None

    | (DialogClosed, CommunityOnSelect community) ->
        DialogClosed, [], CommunitySelected community |> Some

    | (DialogClosed, CommunityOnDelete communityNameToDelete) ->
        DialogClosed, [], deleteCommunityFromSettings currentSettings communityNameToDelete |> SettingsUpdated |> Some

    | (AddingCommunity dialogState, PlayerSelectorMsg playerSelectorMsg) ->
        match (dialogState, playerSelectorMsg) with
        | (_, PlayerSelector.Msg.Abort) -> DialogClosed, [], None
        | (_, PlayerSelector.Msg.Confirm (communityName, playerName)) ->
            DialogClosed, [], addOrUpdateCommunityFromSettings currentSettings { CommunityName = communityName; PlayerName = playerName } |> SettingsUpdated |> Some
        | (_, otherMsg) ->
            let newDialogState, cmdMsgList = PlayerSelector.performTransition dialogState otherMsg
            AddingCommunity newDialogState, cmdMsgList |> List.map PlayerSelectorCmdMsg, None
    
    | (_, _) -> model, [], None

let dialogIsOpen =
    function
    | DialogClosed _ -> false
    | _ -> true

let dialogBackdrop (isVisible: bool): ViewElement =
    View.StackLayout(
        isVisible = isVisible,
        backgroundColor = Color.Black,
        orientation = StackOrientation.Vertical,
        verticalOptions = LayoutOptions.FillAndExpand,
        opacity = 0.5,
        children = [
            View.Label(verticalOptions = LayoutOptions.FillAndExpand)
        ]
    )

let dialogBody (model: Model) dispatch =
    View.StackLayout(
        isVisible = dialogIsOpen model,
        margin = Thickness(30.0, 50.0, 30.0, 50.0),
        backgroundColor = Color.White,
        orientation = StackOrientation.Vertical,
        horizontalOptions = LayoutOptions.FillAndExpand,
        verticalOptions = LayoutOptions.FillAndExpand,
        children =
            match model with
            | DialogClosed -> []
            | AddingCommunity playerSelectorModel -> [ PlayerSelector.view playerSelectorModel dispatch ]
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
        (title = "Settings", icon = ImagePath "tab_settings.png",
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
                       dialogBackdrop (model |> dialogIsOpen)
                       dialogBody model (PlayerSelectorMsg >> dispatch) ]))
