module Relogify.AddResult

open System
open Fabulous
open Fabulous.XamarinForms
open Relogify.ApplicationSettings
open Relogify.ChallengeManager
open Xamarin.Forms
open Relogify.Graphql

type ChallengeState =
    | NotChallenged
    | ReceivedChallenge of notificationId: int
    | WaitingForResponse

type ResultModel =
    { OwnPoints: int
      OpponentPoints: int
      ExtraTime: bool }

type ChallengeModel =
    { IsSendingChallenge: bool
      Challenges: Challenge list }

type State =
    | EditingResult
    | AddingResult
    | ErrorAddingResult of string

type Model =
    { resultModel: ResultModel
      challengeModel: ChallengeModel
      state: State }

let getChallengeState (challenges: Challenge list) (playerInCommunity: PlayerInCommunity) =
    challenges
    |> List.tryFind (fun c -> c.PlayerInCommunity = playerInCommunity)
    |> Option.map (fun challenge ->
        match challenge.Type with
        | Outgoing -> WaitingForResponse
        | Incoming notificationId -> ReceivedChallenge notificationId)
    |> Option.defaultValue NotChallenged

let initModel () =
    // TODO: Command to read challenges instead
    let challenges = getChallenges ()

    { resultModel =
        { OwnPoints = 0
          OpponentPoints = 0
          ExtraTime = false }
      challengeModel =
        { IsSendingChallenge = false
          Challenges = challenges }
      state = EditingResult }

type ChallengeMsg =
    | InitiateChallenge
    | RemoveChallenge of notificationId: int
    | CancelChallenge
    | ConfirmChallenge of notificationId: int
    | ChallengeInitiated of Challenge list
    | ChallengeAccepted

type Msg =
    | ChallengeMessage of ChallengeMsg
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
    | ConfirmChallengeCmdMsg of notificationId: int * fromPlayer: string * toPlayer: string * communityName : string
    | RemoveChallengeCmdMsg of notificationId: int * fromPlayer: string * toPlayer: string * communityName : string
    | CancelChallengeCmdMsg of fromPlayer: string * toPlayer: string * communityName : string

let addResultCmd (resultModel: AddResultModel) =
    async {
        do! Async.SwitchToThreadPool()

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
        do! Async.SwitchToThreadPool()

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
let confirmChallengeCmd (notificationId: int) (fromPlayer: string) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = fromPlayer; CommunityName = communityName }

        // TODO: Move out of AddResult?
        let messagingService = DependencyService.Get<IMessagingService>()
        messagingService.CancelNotification notificationId

        do! ChallengeManager.respondToChallenge fromPlayer communityName
        return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let removeChallengeCmd (notificationId: int) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = toPlayer; CommunityName = communityName }

        // TODO: Move out of AddResult?
        let messagingService = DependencyService.Get<IMessagingService>()
        messagingService.CancelNotification notificationId

        return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let cancelChallengeCmd (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = toPlayer; CommunityName = communityName }
        return ChallengeInitiated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | AddResultCmdMsg resultModel -> addResultCmd resultModel
    | InitiateChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> initiateChallengeCmd fromPlayer toPlayer communityName |> Cmd.map ChallengeMessage
    | ConfirmChallengeCmdMsg (notificationId, fromPlayer, toPlayer, communityName) -> confirmChallengeCmd notificationId fromPlayer toPlayer communityName |> Cmd.map ChallengeMessage
    | RemoveChallengeCmdMsg (notificationId, fromPlayer, toPlayer, communityName) -> removeChallengeCmd notificationId toPlayer communityName |> Cmd.map ChallengeMessage
    | CancelChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> cancelChallengeCmd toPlayer communityName |> Cmd.map ChallengeMessage

let toAddResultModel playerName opponentName communityName (resultModel: ResultModel): AddResultModel =
    { PlayerName = playerName
      OpponentName = opponentName
      CommunityName = communityName
      PlayerPoints = resultModel.OwnPoints
      OpponentPoints = resultModel.OpponentPoints
      ExtraTime = resultModel.ExtraTime }

let hasChallenged (challenges: Challenge list) (playerInCommunity: PlayerInCommunity): bool =
    challenges |> List.exists (fun c -> c.PlayerInCommunity = playerInCommunity)

let updateChallengeModel (ownName: string) (opponentName: string) (communityName: string) (challengeModel: ChallengeModel) (challengeMsg: ChallengeMsg) =
    match challengeMsg with
    | InitiateChallenge -> { challengeModel with IsSendingChallenge = true }, [CmdMsg.InitiateChallengeCmdMsg (ownName, opponentName, communityName)]
    | ConfirmChallenge notificationId -> { challengeModel with IsSendingChallenge = false }, [CmdMsg.ConfirmChallengeCmdMsg (notificationId, opponentName, ownName, communityName)]
    | RemoveChallenge notificationId -> { challengeModel with IsSendingChallenge = true }, [CmdMsg.RemoveChallengeCmdMsg (notificationId, ownName, opponentName, communityName)]
    | CancelChallenge -> { challengeModel with IsSendingChallenge = true }, [CmdMsg.CancelChallengeCmdMsg (ownName, opponentName, communityName)]
    | ChallengeInitiated newChallengeList -> { challengeModel with IsSendingChallenge = false; Challenges = newChallengeList }, [] //, Some ChallengesUpdated
    | ChallengeAccepted -> challengeModel, [] //, Some ChallengesUpdated

let update (model: Model) (msg: Msg) (communityName: string) (ownName: string) (opponentName: string): Model * CmdMsg list =
    match model.state, msg with
    | (_, ChallengeMessage challengeMsg) ->
        let challengeModel, cmdMsgs = updateChallengeModel ownName opponentName communityName model.challengeModel challengeMsg
        { model with state = EditingResult; challengeModel = challengeModel }, cmdMsgs

    | (EditingResult, SetOwnPoints newOwnPoints)
    | (ErrorAddingResult _, SetOwnPoints newOwnPoints) ->
        { model with state = EditingResult; resultModel = { model.resultModel with OwnPoints = newOwnPoints }}, []

    | (EditingResult, SetOpponentPoints newOpponentPoints)
    | (ErrorAddingResult _, SetOpponentPoints newOpponentPoints) ->
        { model with state = EditingResult; resultModel = { model.resultModel with OpponentPoints = newOpponentPoints }}, []

    | (EditingResult, ToggleExtraTime)
    | (ErrorAddingResult _, ToggleExtraTime) ->
        { model with state = EditingResult; resultModel = { model.resultModel with ExtraTime = not model.resultModel.ExtraTime }}, []

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

let applyImageButtonStyle (imageButton: ViewElement) =
    imageButton
        .ImageButtonCornerRadius(16)
        .WidthRequest(32.0)
        .HeightRequest(32.0)

let challengeIcon (challengeState: ChallengeState) (isSendingChallenge: bool) (dispatch: Msg -> unit) =
    if isSendingChallenge then
        View.ActivityIndicator(
           height = 32.0,
           width = 32.0,
           isRunning = true
        )
    else
        match challengeState with
        | NotChallenged ->
            View.ImageButton(
                source = Image.fromPath "target_icon.png",
                backgroundColor = Color.Wheat,
                command = (fun _ -> dispatch <| ChallengeMessage InitiateChallenge)
            ) |> applyImageButtonStyle
        | WaitingForResponse ->
            View.ImageButton(
                source = Image.fromPath "decline_icon.png",
                backgroundColor = Color.LightPink,
                command = (fun _ -> dispatch <| ChallengeMessage CancelChallenge)
            ) |> applyImageButtonStyle
        | ReceivedChallenge notificationId ->
            View.StackLayout(
                orientation = StackOrientation.Horizontal,
                children = [
                    View.ImageButton(
                        source = Image.fromPath "accept_icon.png",
                        backgroundColor = Color.LightGreen,
                        command = (fun _ -> dispatch <| ChallengeMessage (ConfirmChallenge notificationId))
                    ) |> applyImageButtonStyle
                    View.ImageButton(
                        source = Image.fromPath "decline_icon.png",
                        backgroundColor = Color.LightPink,
                        command = (fun _ -> dispatch <| ChallengeMessage (RemoveChallenge notificationId))
                    ) |> applyImageButtonStyle
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
                                     challengeIcon (getChallengeState model.challengeModel.Challenges { PlayerName = opponentName; CommunityName = communityName }) model.challengeModel.IsSendingChallenge dispatch
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
