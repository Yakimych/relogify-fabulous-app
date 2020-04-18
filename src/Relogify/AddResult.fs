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
    | AddResultCmdMsg of AddResultModel

let addResultCmd (resultModel: AddResultModel) =
    async {
        let currentDateString = DateTime.UtcNow.ToString("o")

        let! result =
            addResultOperation.AsyncRun(
                runtimeContext,
                resultModel.CommunityName,
                resultModel.PlayerName,
                resultModel.OpponentName,
                currentDateString,
                resultModel.PlayerPoints,
                resultModel.OpponentPoints,
                resultModel.ExtraTime
            )

        return
            match result.Data with
            | Some _ -> ResultAddedSuccess
            | None -> ResultAddedError "Failed to add result, please check your internet connection and try again"
    }
    |> Cmd.ofAsyncMsg

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | AddResultCmdMsg resultModel -> addResultCmd resultModel

let toAddResultModel playerName opponentName communityName (resultModel: ResultModel): AddResultModel =
    { PlayerName = playerName
      OpponentName = opponentName
      CommunityName = communityName
      PlayerPoints = resultModel.OwnPoints
      OpponentPoints = resultModel.OpponentPoints
      ExtraTime = resultModel.ExtraTime }

let updateResultModel (resultModel: ResultModel) (msg: Msg): ResultModel =
    match msg with
    | SetOwnPoints newOwnPoints -> { resultModel with OwnPoints = newOwnPoints }
    | SetOpponentPoints newOpponentPoints -> { resultModel with OpponentPoints = newOpponentPoints }
    | ToggleExtraTime -> { resultModel with ExtraTime = not resultModel.ExtraTime }
    | _ -> resultModel

let update (model: Model) (msg: Msg) (communityName: string) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match model.state, msg with
    | (EditingResult, SetOwnPoints _)
    | (EditingResult, SetOpponentPoints _)
    | (EditingResult, ToggleExtraTime)
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

let getIntValueOrZero (stringValue: string): int =
    let succeeded, intValue = Int32.TryParse(stringValue)
    if succeeded then intValue else 0

let maxSelectablePoints = 100

let applyBaseButtonStyle (button: ViewElement) =
    button
        .ButtonCornerRadius(10)
        .BorderWidth(2.0)
        .BorderColor(Color.Black)
        .FontSize(Named(NamedSize.Large))

let pointSelector (selectedNumberOfPoints: int) setPoints =
    View.ScrollView(
        content =
            View.StackLayout(
                orientation = StackOrientation.Vertical,
                children = (
                    [0 .. maxSelectablePoints]
                    |> List.map (fun i ->
                        View.Button(
                            text = i.ToString(),
                            backgroundColor = (if i = selectedNumberOfPoints then Color.LightGreen else Color.LightGray),
                            textColor = Color.Black,
                            margin = Thickness(15.0, 5.0, 15.0, 0.0),
                            command = (fun _ -> setPoints i)
                        ) |> applyBaseButtonStyle
                    )
                )
            )
    )

let view (model: Model) (dispatch: Msg -> unit) (ownName: string) (opponentName: string) =
    let isAddingResult = model.state |> isAddingResult

    View.ContentPage(
        title = "Add Result",
        content =
            View.Grid(
                padding = Thickness(15.0),
                margin = Thickness(15.0),
                rowdefs = [Star; Auto; Auto; Auto],
                children = [
                    View.Grid(
                         coldefs = [ Star; Star ],
                         rowdefs = [ Auto; Auto; Star ],
                         children = [
                             View.Label(lineBreakMode = LineBreakMode.TailTruncation, text = ownName, fontSize = FontSize.Named(NamedSize.Large)).Column(0).Row(0)
                             View.Label(lineBreakMode = LineBreakMode.TailTruncation, text = opponentName, fontSize = FontSize.Named(NamedSize.Large)).Column(1).Row(0)

                             View.Entry(
                                 isEnabled = not isAddingResult,
                                 keyboard = Keyboard.Numeric,
                                 text = model.resultModel.OwnPoints.ToString(),
                                 textChanged = (fun args -> dispatch <| SetOwnPoints (getIntValueOrZero args.NewTextValue))
                             ).Column(0).Row(1)
                             View.Entry(
                                 isEnabled = not isAddingResult,
                                 keyboard = Keyboard.Numeric,
                                 text = model.resultModel.OpponentPoints.ToString(),
                                 textChanged = (fun args -> dispatch <| SetOpponentPoints (getIntValueOrZero args.NewTextValue))
                             ).Column(1).Row(1)

                             (pointSelector model.resultModel.OwnPoints (dispatch << SetOwnPoints)).Column(0).Row(2)
                             (pointSelector model.resultModel.OpponentPoints (dispatch << SetOpponentPoints)).Column(1).Row(2)
                         ]
                    ).Row(0)

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
                    ).Row(1)

                    View.Grid(
                        children = [
                            View.Button(
                                text = "Add Result",
                                backgroundColor = Color.Orange,
                                textColor = Color.DarkBlue,
                                height = 60.0,
                                command = (fun _ -> dispatch AddResultInitiated),
                                commandCanExecute = not isAddingResult
                            ) |> applyBaseButtonStyle
                            View.ActivityIndicator(
                               horizontalOptions = LayoutOptions.End,
                               margin = Thickness(0.0, 0.0, 40.0, 0.0),
                               isVisible = isAddingResult,
                               isRunning = isAddingResult
                            )
                        ]
                    ).Row(2)

                    View.Label(text = getErrorMessage model, textColor = Color.Red).Row(3)
                ]
            )
    )
