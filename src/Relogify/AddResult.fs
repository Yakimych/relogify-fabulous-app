module Relogify.AddResult

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { OwnName: string
      OpponentName: string
      OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool
      CanAddResult: bool }

let initModel =
    { OwnName = "Player1"
      OpponentName = "Player2"
      OwnPoints = 0
      OpponentPoints = 0
      CanAddResult = false
      ExtraTime = false }

type Msg =
    | SetOwnPoints of int
    | SetOpponentPoints of int
    | ToggleExtraTime

let update model msg =
    match msg with
    | SetOwnPoints newOwnPoints -> { model with OwnPoints = newOwnPoints }, Cmd.none
    | SetOpponentPoints newOpponentPoints -> { model with OpponentPoints = newOpponentPoints }, Cmd.none
    | ToggleExtraTime -> { model with ExtraTime = not model.ExtraTime }, Cmd.none

let getTitle = sprintf "Playing against %s"

// TODO: Show this via View.MasterDetailPage?
let view model dispatch =
    View.ContentPage(
        title = getTitle model.OpponentName,
        content = View.CollectionView(
            items = [
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = Thickness(15.0),
                    margin = Thickness(15.0),
                    children = [
                        View.Grid(
                             coldefs = [ Star; Star ],
                             rowdefs = [ Auto; Auto; Star ],
                             children = [
                                 View.Label(text = model.OwnName, fontSize = FontSize.Named(NamedSize.Small)).Column(0).Row(0)
                                 View.Label(text = model.OpponentName, fontSize = FontSize.Named(NamedSize.Small)).Column(1).Row(0)

                                 View.Picker(
                                     title = "My Points",
                                     items = ([1 .. 10] |> List.map(fun i -> i.ToString())),
                                     selectedIndex = model.OwnPoints // TODO: "Bind" to the value rather than the index
                                 ).Column(0).Row(1)
                                 View.Picker(
                                     title = "Opponent Points",
                                     items = ([1 .. 10] |> List.map(fun i -> i.ToString())),
                                     selectedIndex = model.OpponentPoints // TODO: "Bind" to the value rather than the index
                                 ).Column(1).Row(1)
                             ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            children = [
                                View.Label(text = "Extra time", verticalTextAlignment = TextAlignment.Center)
                                View.CheckBox(margin = Thickness(0.0), isChecked = model.ExtraTime, checkedChanged = (fun _ -> dispatch ToggleExtraTime))
                            ]
                        )

                        View.Button(
                            text = "Add Result",
                            isEnabled = model.CanAddResult,
                            backgroundColor = Color.Orange,
                            textColor = Color.DarkBlue
                            // TODO: Add command
                        )

                    ]
                )
            ]
        )
    ).ToolbarItems([View.ToolbarItem(text = "Timer", command = (fun () -> ()))])
