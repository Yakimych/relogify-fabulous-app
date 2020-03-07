module Relogify.Settings

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { CommunityName: string
      PlayerName: string
      DraftCommunityName: string
      DraftPlayerName: string
      CommunityDialogIsOpen : bool
      ShowChooseCommunity : bool
      CanCancelDialog : bool
      IsChoosingPlayer : bool
      CanTriggerLoadPlayers : bool
      IsLoadingPlayers : bool }

let initModel =
    { CommunityName = "testcommunity"
      PlayerName = "testplayer"
      DraftCommunityName = "testcommunity"
      DraftPlayerName = "testplayer"
      CommunityDialogIsOpen = true
      ShowChooseCommunity = true
      CanCancelDialog = true
      IsChoosingPlayer = false // TODO: This should be a function calculating the value based on the current view state
      CanTriggerLoadPlayers = false
      IsLoadingPlayers = false }

type Msg =
    | CommunityNameChanged of string
    | PlayerNameChanged of string
    | FetchPlayersClicked
    | PlayersFetched of string

let fetchPlayersCmd =
    async {
        do! Async.Sleep 200
        return Msg.PlayersFetched "players"
    }
    |> Cmd.ofAsyncMsg

let update model msg =
    match msg with
    | CommunityNameChanged newCommunityName -> { model with CommunityName = newCommunityName }, Cmd.none
    | PlayerNameChanged newPlayerName -> { model with PlayerName = newPlayerName }, Cmd.none
    | FetchPlayersClicked -> model, fetchPlayersCmd
    | PlayersFetched players -> { model with PlayerName = players }, Cmd.none

// TODO: Break up into smaller functions
let view model dispatch =
    View.ContentPage(
        title = "Settings",
        content = View.CollectionView(
            items = [
                View.Grid
                    (margin = Thickness(10.0),
                     coldefs = [ Auto; Star ],
                     rowdefs = [ for _ in 1 .. 3 -> Auto ],
                     children =
                         [ View.Label(text = "Community: ", fontSize = FontSize.Named(NamedSize.Large),
                                      margin = Thickness(left = 15.0, top = 0.0, right = 0.0, bottom = 0.0)).Row(0).Column(0)
                           View.Label(text = model.CommunityName, fontSize = FontSize.Named(NamedSize.Large),
                                      margin = Thickness(left = 15.0, top = 0.0, right = 0.0, bottom = 0.0)).Row(0).Column(1)
                           View.Label(text = "Player: ", fontSize = FontSize.Named(NamedSize.Large),
                                      margin = Thickness(left = 15.0, top = 0.0, right = 0.0, bottom = 0.0)).Row(1).Column(0)
                           View.Label(text = model.PlayerName, fontSize = FontSize.Named(NamedSize.Large),
                                      margin = Thickness(left = 15.0, top = 0.0, right = 0.0, bottom = 0.0)).Row(1).Column(1)
                           View.Button(text = "Edit", backgroundColor = Color.Orange, textColor = Color.Black,
                                       fontSize = FontSize.Named(NamedSize.Large),
                                       margin = Thickness(15.0), height = 60.0, cornerRadius = 10, borderWidth = 2.0).Row(2).Column(0)
                               .ColumnSpan(2) ])

                View.StackLayout(
                    isVisible = model.CommunityDialogIsOpen,
                    backgroundColor = Color.Black,
                    opacity = 0.5
                )

                View.StackLayout(
                    isVisible = model.CommunityDialogIsOpen,
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
                                         View.StackLayout(
                                             margin = Thickness(10.0),
                                             isVisible = model.ShowChooseCommunity,
                                             children = [
                                                 View.Label(text = "Step 1: Enter the community name")
                                                 View.Entry(text = model.DraftCommunityName) // TODO: , keyboard = Entry.KeyboardProperty())
                                                 View.Label(text = "The community name is the last part of your Relogify URL: ")
                                                 View.Label(text = "TODO: Formatted text")
                                                 View.Grid(children = [
                                                     View.Button(text = "Fetch Players", isEnabled = model.CanTriggerLoadPlayers, backgroundColor = Color.LightGreen)
                                                     View.ActivityIndicator(
                                                        horizontalOptions = LayoutOptions.End,
                                                        margin = Thickness(0.0, 0.0, 40.0, 0.0),
                                                        isVisible = model.IsLoadingPlayers,
                                                        isRunning = model.IsLoadingPlayers
                                                     )
                                                 ])
                                             ]
                                         ).Row(0)

                                         View.StackLayout(
                                             margin = Thickness(10.0),
                                             isVisible = model.IsChoosingPlayer,
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

                                         View.Button(
                                             text = "Cancel",
                                             isVisible = model.CanCancelDialog,
                                             backgroundColor = Color.LightGray
                                         ).Row(1)
                                     ]
                                 )
                            ]
                        )
                    ]
                )
            ]
        )
    )
