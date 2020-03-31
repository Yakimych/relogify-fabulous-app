module Relogify.AddResult

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Relogify.Graphql

type ResultModel =
    { OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool }

type State =
    | EditingResult
    | AddingResult
    | ErrorAddingResult of string

type Model =
    { resultModel: ResultModel
      state: State }

let initModel =
    { resultModel =
        { OwnPoints = 0
          OpponentPoints = 0
          ExtraTime = false }
      state = EditingResult }

type Msg =
    | SetOwnPoints of int
    | SetOpponentPoints of int
    | ToggleExtraTime
    | AddResultInitiated
    | ResultAddedSuccess
    | ResultAddedError of string

type AddResultModel =
    { CommunityName: string
      PlayerName: string
      OpponentName: string
      PlayerPoints: int
      OpponentPoints: int
      ExtraTime: bool }

type CmdMsg =
    | Noop
    | AddResultCmdMsg of AddResultModel

let rand = Random()

let addResultCmd (resultModel: AddResultModel) =
    async {
        // replace getPlayersOperation with addResultOperation
        do! Async.Sleep 1000
        let! result = getPlayersOperation.AsyncRun(runtimeContext, resultModel.OpponentName) // TODO: Send in the real data
        return
            match result.Data with // TODO: result.Errors
            | Some _ -> // ResultAddedSuccess // TODO: Uncomment this and remove the randomness
                if rand.Next(2) = 0 then
                    ResultAddedError "Failed to add result, please check your internet connection and try again"
                else
                    ResultAddedSuccess
            | None -> ResultAddedError "Failed to add result, please check your internet connection and try again"
    }
    |> Cmd.ofAsyncMsg

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | Noop -> Cmd.none
    | AddResultCmdMsg resultModel -> addResultCmd resultModel

let toAddResultModel (playerName: string) (opponentName: string) (communityName: string) (resultModel: ResultModel): AddResultModel =
    { PlayerName = playerName
      OpponentName = opponentName
      CommunityName = communityName
      PlayerPoints = resultModel.OwnPoints
      OpponentPoints = resultModel.OpponentPoints
      ExtraTime = resultModel.ExtraTime }

let updateResultModel (resultModel: ResultModel) (msg: Msg): ResultModel =
    match msg with
    | SetOwnPoints newOwnPoints -> { resultModel with OpponentPoints = newOwnPoints }
    | SetOpponentPoints newOpponentPoints -> { resultModel with OpponentPoints = newOpponentPoints }
    | ToggleExtraTime -> { resultModel with ExtraTime = not resultModel.ExtraTime }
    | _ -> resultModel

let update (model: Model) (msg: Msg) (communityName: string) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match model.state, msg with
    | (EditingResult, SetOwnPoints _)
    | (EditingResult, SetOpponentPoints _)
    | (EditingResult, ToggleExtraTime) -> { model with resultModel = updateResultModel model.resultModel msg }, []

    | (ErrorAddingResult _, SetOwnPoints _)
    | (ErrorAddingResult _, SetOpponentPoints _)
    | (ErrorAddingResult _, ToggleExtraTime) -> { model with state = EditingResult; resultModel = updateResultModel model.resultModel msg }, []

    | (ErrorAddingResult _, AddResultInitiated)
    | (EditingResult, AddResultInitiated) -> { model with state = AddingResult }, [AddResultCmdMsg (toAddResultModel ownName opponentName communityName model.resultModel)]

    | (AddingResult, ResultAddedError errorMessage) -> { model with state = ErrorAddingResult errorMessage }, []

    | _ -> model, []

let navigationPrimaryColor = Color.FromHex("#2196F3")

let isAddingResult (state: State): bool =
    match state with
    | AddingResult -> true
    | _ -> false

let getErrorMessage (model: Model): string =
    match model.state with
    | ErrorAddingResult errorMessage -> errorMessage
    | _ -> ""

let view (model: Model) (dispatch: Msg -> unit) (ownName: string) (opponentName: string) =
    let isAddingResult = model.state |> isAddingResult

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
                                     isEnabled = not isAddingResult,
                                     items = ([1 .. 10] |> List.map(fun i -> i.ToString())),
                                     selectedIndex = model.resultModel.OwnPoints // TODO: "Bind" to the value rather than the index
                                 ).Column(0).Row(1)
                                 View.Picker(
                                     isEnabled = not isAddingResult,
                                     items = ([1 .. 10] |> List.map(fun i -> i.ToString())),
                                     selectedIndex = model.resultModel.OpponentPoints // TODO: "Bind" to the value rather than the index
                                 ).Column(1).Row(1)
                             ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            children = [
                                View.Label(text = "Extra time", verticalTextAlignment = TextAlignment.Center)
                                View.CheckBox(
                                    isEnabled = not isAddingResult,
                                    margin = Thickness(0.0),
                                    isChecked = model.resultModel.ExtraTime,
                                    checkedChanged = (fun _ -> dispatch ToggleExtraTime)
                                )
                            ]
                        )

                        View.Grid(children = [
                            View.Button(
                                text = "Add Result",
                                backgroundColor = Color.Orange,
                                textColor = Color.DarkBlue,
                                command = (fun _ -> dispatch AddResultInitiated),
                                commandCanExecute = not isAddingResult
                            )
                            View.ActivityIndicator(
                               horizontalOptions = LayoutOptions.End,
                               margin = Thickness(0.0, 0.0, 40.0, 0.0),
                               isVisible = isAddingResult,
                               isRunning = isAddingResult
                            )
                        ])

                        View.Label(text = getErrorMessage model, textColor = Color.Red)
                    ]
                )
            ]
        )
    )
