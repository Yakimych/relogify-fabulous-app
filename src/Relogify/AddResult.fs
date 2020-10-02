module Relogify.AddResult

open System
open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Xamarin.Forms
open Relogify.Graphql

type ChallengeModel =
    | NotChallenged
    | ReceivedChallenge
    | WaitingForResponse

type ResultModel =
    { IsSendingChallenge: bool
      Challenges: Challenge list
      OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool }

type State =
    | EditingResult
    | AddingResult
    | ErrorAddingResult of string

type Model =
    { resultModel: ResultModel
      state: State }

let getChallengeState (challenges: Challenge list) (playerInCommunity: PlayerInCommunity) =
    // TODO: Refactor
    let maybeChallenge = challenges |> List.tryFind (fun c -> c.PlayerInCommunity = playerInCommunity)
    match maybeChallenge with
    | None -> NotChallenged
    | Some challenge ->
        match challenge.Type with
        | Outgoing -> WaitingForResponse
        | Incoming -> ReceivedChallenge

let initModel () =
    // TODO: Command to read challenges instead
    let challenges = getChallenges ()

    { resultModel =
        { IsSendingChallenge = false
          Challenges = challenges
          OwnPoints = 0
          OpponentPoints = 0
          ExtraTime = false }
      state = EditingResult }

type Msg =
    | InitiateChallenge
    | RemoveChallenge
    | ConfirmChallenge
    | ChallengeInitiated of Challenge list
    | ChallengeAccepted
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
    | InitiateChallengeCmdMsg of fromPlayer: string * toPlayer: string * communityName : string
    | ConfirmChallengeCmdMsg of fromPlayer: string * toPlayer: string * communityName : string
    | RemoveChallengeCmdMsg of fromPlayer: string * toPlayer: string * communityName : string

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

        // TODO: Remove challenge(s) for this opponent from Local storage

        return
            match result.Data with
            | Some _ -> ResultAddedSuccess
            | None -> ResultAddedError "Failed to add result, please check your internet connection and try again"
    }
    |> Cmd.ofAsyncMsg

let initiateChallengeCmd (fromPlayer: string) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = addChallengeToLocalStorage { PlayerName = toPlayer; CommunityName = communityName } Outgoing
        let! response = ChallengeManager.performChallengeApiCall fromPlayer toPlayer communityName

        if response.StatusCode >= 200 && response.StatusCode < 300 then
            return ChallengeInitiated newChallengeList
        else
            // TODO: Handle "Challenge failed"
            return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

// TODO: Remove duplication
let confirmChallengeCmd (fromPlayer: string) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = fromPlayer; CommunityName = communityName }
        do! ChallengeManager.respondToChallenge fromPlayer communityName
        return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let removeCancelChallengeCmd (fromPlayer: string) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = toPlayer; CommunityName = communityName }

        // TODO: Is there a way to remove the "correct" notification too?

        return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | AddResultCmdMsg resultModel -> addResultCmd resultModel
    | InitiateChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> initiateChallengeCmd fromPlayer toPlayer communityName
    | ConfirmChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> confirmChallengeCmd fromPlayer toPlayer communityName
    | RemoveChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> removeCancelChallengeCmd fromPlayer toPlayer communityName

let toAddResultModel playerName opponentName communityName (resultModel: ResultModel): AddResultModel =
    { PlayerName = playerName
      OpponentName = opponentName
      CommunityName = communityName
      PlayerPoints = resultModel.OwnPoints
      OpponentPoints = resultModel.OpponentPoints
      ExtraTime = resultModel.ExtraTime }

let hasChallenged (challenges: Challenge list) (playerInCommunity: PlayerInCommunity): bool =
    challenges |> List.exists (fun c -> c.PlayerInCommunity = playerInCommunity)

let update (model: Model) (msg: Msg) (communityName: string) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match model.state, msg with
    | (EditingResult, SetOwnPoints _)
    | (EditingResult, SetOpponentPoints _)
    | (EditingResult, ToggleExtraTime)
    | (EditingResult, InitiateChallenge)
    | (EditingResult, ConfirmChallenge)
    | (EditingResult, RemoveChallenge)
    | (EditingResult, ChallengeInitiated _)
    | (EditingResult, ChallengeAccepted)
    | (ErrorAddingResult _, SetOwnPoints _)
    | (ErrorAddingResult _, SetOpponentPoints _)
    | (ErrorAddingResult _, ToggleExtraTime) ->
        // TODO: Separate update function for handling challenge flow?
        let (updatedResultModel, cmdMsgs) =
            match msg with
            | InitiateChallenge -> { model.resultModel with IsSendingChallenge = true }, [CmdMsg.InitiateChallengeCmdMsg (ownName, opponentName, communityName)]
            | ConfirmChallenge -> { model.resultModel with IsSendingChallenge = false }, [CmdMsg.ConfirmChallengeCmdMsg (opponentName, ownName, communityName)]
            | RemoveChallenge -> { model.resultModel with IsSendingChallenge = true }, [CmdMsg.RemoveChallengeCmdMsg (ownName, opponentName, communityName)]
            | ChallengeInitiated newChallengeList -> { model.resultModel with IsSendingChallenge = false; Challenges = newChallengeList }, [] //, Some ChallengesUpdated
            | ChallengeAccepted -> model.resultModel, [] //, Some ChallengesUpdated
            | SetOwnPoints newOwnPoints -> { model.resultModel with OwnPoints = newOwnPoints }, []
            | SetOpponentPoints newOpponentPoints -> { model.resultModel with OpponentPoints = newOpponentPoints }, []
            | ToggleExtraTime -> { model.resultModel with ExtraTime = not model.resultModel.ExtraTime }, []
            | AddResultInitiated -> model.resultModel, []
            | ResultAddedSuccess _ -> model.resultModel, []
            | ResultAddedError _ -> model.resultModel, []
        { model with state = EditingResult; resultModel = updatedResultModel }, cmdMsgs

    | (ErrorAddingResult _, AddResultInitiated)
    | (EditingResult, AddResultInitiated) -> { model with state = AddingResult }, [AddResultCmdMsg (toAddResultModel ownName opponentName communityName model.resultModel)]

    | (AddingResult, ResultAddedError errorMessage) -> { model with state = ErrorAddingResult errorMessage }, []

    | _ -> model, []

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
        .FontSize(FontSize.fromNamedSize(NamedSize.Large))

let applySmallButtonStyle (button: ViewElement) =
    button
        .BorderWidth(1.0)
        .BorderColor(Color.Black)
        .FontSize(FontSize.fromNamedSize(NamedSize.Micro))

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

let challengeIcon (challengeState: ChallengeModel) (isSendingChallenge: bool) (dispatch: Msg -> unit) =
    match (challengeState, isSendingChallenge) with
    | NotChallenged, _ ->
        View.ImageButton(
            source = Image.fromPath "tab_about.png",
            backgroundColor = Color.Orange,
            command = (fun _ -> dispatch InitiateChallenge)
        )
    | WaitingForResponse, true -> // TODO: Spinner
        View.ImageButton(
            source = Image.fromPath "tab_about.png",
            backgroundColor = Color.WhiteSmoke,
            commandCanExecute = false
        )
    | WaitingForResponse, false -> // TODO: Bake in the boolean into WaitingForResponse?
        View.ImageButton(
            source = Image.fromPath "tab_about.png",
            backgroundColor = Color.WhiteSmoke,
            commandCanExecute = false
        )
    | ReceivedChallenge, _ ->
        View.ImageButton(
            source = Image.fromPath "tab_feed.png",
            backgroundColor = Color.Gray,
            commandCanExecute = false
        )

let challengeText (challengeState: ChallengeModel) (isSendingChallenge: bool) (dispatch: Msg -> unit) =
    if isSendingChallenge then
        View.Label(
            text = "...",
            backgroundColor = Color.WhiteSmoke
        )
    else
        match challengeState with
        | NotChallenged ->
            View.Button(
                text = "C!",
                backgroundColor = Color.Orange,
                command = (fun _ -> dispatch InitiateChallenge)
            )
        | WaitingForResponse ->
            View.Button(
                text = "X",
                backgroundColor = Color.WhiteSmoke,
                command = (fun _ -> dispatch RemoveChallenge)
            )
        | ReceivedChallenge ->
            View.StackLayout(
                orientation = StackOrientation.Horizontal,
                children = [
                    View.Button(
                        text = "Y",
                        backgroundColor = Color.Blue,
                        command = (fun _ -> dispatch ConfirmChallenge)
                    )
                    View.Button(
                        text = "N",
                        backgroundColor = Color.Red,
                        command = (fun _ -> dispatch RemoveChallenge)
                    )
                ])

let view (model: Model) (dispatch: Msg -> unit) (ownName: string) (opponentName: string) (communityName: string) =
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
                             View.Label(lineBreakMode = LineBreakMode.TailTruncation, text = ownName, fontSize = FontSize.fromNamedSize(NamedSize.Large)).Column(0).Row(0)
                             View.StackLayout(
                                 orientation = StackOrientation.Horizontal,
                                 children = [
                                     View.Label(lineBreakMode = LineBreakMode.TailTruncation, text = opponentName, fontSize = FontSize.fromNamedSize(NamedSize.Large))
//                                     challengeIcon (getChallengeState model.resultModel.Challenges { PlayerName = opponentName; CommunityName = communityName }) model.resultModel.IsSendingChallenge dispatch
                                     challengeText (getChallengeState model.resultModel.Challenges { PlayerName = opponentName; CommunityName = communityName }) model.resultModel.IsSendingChallenge dispatch
//                                     View.Button(
//                                         text = "Challenge",
//                                         backgroundColor = Color.Green,
//                                         textColor = Color.DarkBlue,
//                                         height = 20.0
//        //                                 command = (fun _ -> dispatch Challenge),
//        //                                 commandCanExecute = not isAddingResult
//                                     ) |> applySmallButtonStyle
                                ]
                             ).Column(1).Row(0)

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
                            ).Column(0) |> applyBaseButtonStyle
                            View.ActivityIndicator(
                               horizontalOptions = LayoutOptions.End,
                               margin = Thickness(0.0, 0.0, 40.0, 0.0),
                               isVisible = isAddingResult,
                               isRunning = isAddingResult
                            ).Column(0)
                        ]
                    ).Row(2)

                    View.Label(text = getErrorMessage model, textColor = Color.Red).Row(3)
                ]
            )
    )
