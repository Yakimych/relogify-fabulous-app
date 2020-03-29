module Relogify.AddResult

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Relogify.Graphql

type Model =
    { OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool
      IsAddingResult: bool } // TODO: Replace with loading state

let initModel =
    { OwnPoints = 0
      OpponentPoints = 0
      ExtraTime = false
      IsAddingResult = false }

type Msg =
    | SetOwnPoints of int
    | SetOpponentPoints of int
    | ToggleExtraTime
    | AddResultInitiated
    | ResultAddedSuccess
    | ResultAddedError of string

type ResultModel =
    { PlayerName: string
      OpponentName: string
      PlayerPoints: int
      OpponentPoints: int
      ExtraTime: bool }

type CmdMsg =
    | Noop
    | AddResultCmdMsg of ResultModel

let addResultCmd (resultModel: ResultModel) =
    async {
        // replace getPlayersOperation with addResultOperation
        let! result = getPlayersOperation.AsyncRun(runtimeContext, resultModel.OpponentName) // TODO: Send in the real data
        return
            match result.Data with
            | Some _ -> ResultAddedSuccess
            | None -> ResultAddedError "Error adding result" // TODO: Handle errors correctly
    }
    |> Cmd.ofAsyncMsg

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | Noop -> Cmd.none
    | AddResultCmdMsg resultModel -> addResultCmd resultModel

let update (model: Model) (msg: Msg) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match msg with
    | SetOwnPoints newOwnPoints -> { model with OwnPoints = newOwnPoints }, []
    | SetOpponentPoints newOpponentPoints -> { model with OpponentPoints = newOpponentPoints }, []
    | ToggleExtraTime -> { model with ExtraTime = not model.ExtraTime }, []
    | ResultAddedSuccess -> { model with IsAddingResult = false }, []
    | ResultAddedError _ -> { model with IsAddingResult = false }, []
    | AddResultInitiated ->
        let resultModel: ResultModel =
            { PlayerName = ownName
              OpponentName = opponentName
              PlayerPoints = model.OwnPoints
              OpponentPoints = model.OpponentPoints
              ExtraTime = model.ExtraTime }
        { model with IsAddingResult = true }, [AddResultCmdMsg resultModel]

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
                            backgroundColor = Color.Orange,
                            textColor = Color.DarkBlue,
                            command = (fun _ -> dispatch AddResultInitiated),
                            commandCanExecute = not model.IsAddingResult
                        )
                    ]
                )
            ]
        )
    )
