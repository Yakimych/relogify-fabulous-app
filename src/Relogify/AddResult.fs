module Relogify.AddResult

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool
      CanAddResult: bool }

let initModel =
    { OwnPoints = 0
      OpponentPoints = 0
      CanAddResult = false
      ExtraTime = false }

type Msg =
    | SetOwnPoints of int
    | SetOpponentPoints of int
    | ToggleExtraTime

type CmdMsg = Noop

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | Noop -> Cmd.none

let update (model: Model) (msg: Msg) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match msg with
    | SetOwnPoints newOwnPoints -> { model with OwnPoints = newOwnPoints }, []
    | SetOpponentPoints newOpponentPoints -> { model with OpponentPoints = newOpponentPoints }, []
    | ToggleExtraTime -> { model with ExtraTime = not model.ExtraTime; CanAddResult = true }, []

// TODO: Move to parent
//let getTitle = sprintf "Playing against %s"

let navigationPrimaryColor = Color.FromHex("#2196F3")

// TODO: Show this via View.MasterDetailPage?
let view (model: Model) (dispatch: Msg -> unit) (ownName: string) (opponentName: string) =
    View.ContentPage(
        title = "Report",
        icon = Image.Path "tab_feed.png",
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
                                 View.Label(text = ownName, fontSize = FontSize.Named(NamedSize.Small)).Column(0).Row(0)
                                 View.Label(text = opponentName, fontSize = FontSize.Named(NamedSize.Small)).Column(1).Row(0)

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
    )
