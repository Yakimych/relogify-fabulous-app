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
    { IsProcessingChallenge: bool
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

type ChallengeMsg =
    | InitiateChallenge
    | DeclineChallenge of notificationId: int
    | CancelChallenge
    | AcceptChallenge of notificationId: int
    | ChallengesUpdated of Challenge list
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
    | AcceptChallengeCmdMsg of notificationId: int * fromPlayer: string * communityName : string
    | DeclineChallengeCmdMsg of notificationId: int * toPlayer: string * communityName : string
    | CancelChallengeCmdMsg of toPlayer: string * communityName : string
    | ReReadChallengesCmdMsg

let initialModel =
    { resultModel =
        { OwnPoints = 0
          OpponentPoints = 0
          ExtraTime = false }
      challengeModel =
        { IsProcessingChallenge = false
          Challenges = [] }
      state = EditingResult }

let initModel () = (initialModel, [CmdMsg.ReReadChallengesCmdMsg])

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

        let playerToRemoveChallengeFor =
            { PlayerName = resultModel.OpponentName; CommunityName = resultModel.CommunityName }
        do! removeChallengeFromLocalStorage playerToRemoveChallengeFor |> Async.Ignore

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
        let! response = performChallengeApiCall fromPlayer toPlayer communityName

        if response.StatusCode >= 200 && response.StatusCode < 300 then
            return ChallengesUpdated newChallengeList
        else
            return ChallengesUpdated newChallengeList // TODO: Handle "Challenge failed"
    }
    |> Cmd.ofAsyncMsg

let acceptChallengeCmd (notificationId: int) (fromPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = fromPlayer; CommunityName = communityName }
        cancelNotification notificationId

        do! respondToChallengeApiCall ChallengeResponse.Accept notificationId |> Async.Ignore
        return ChallengesUpdated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let declineChallengeCmd (notificationId: int) (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = toPlayer; CommunityName = communityName }
        cancelNotification notificationId

        do! respondToChallengeApiCall ChallengeResponse.Decline notificationId |> Async.Ignore
        return ChallengesUpdated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let cancelChallengeCmd (toPlayer: string) (communityName: string) =
    async {
        let! newChallengeList = removeChallengeFromLocalStorage { PlayerName = toPlayer; CommunityName = communityName }
        return ChallengesUpdated newChallengeList
    }
    |> Cmd.ofAsyncMsg

let reReadChallengesCmd () =
    let challenges = getChallenges ()
    Cmd.ofMsg <| ChallengesUpdated challenges

let mapCommands: CmdMsg -> Cmd<Msg> =
    function
    | AddResultCmdMsg resultModel -> addResultCmd resultModel
    | InitiateChallengeCmdMsg (fromPlayer, toPlayer, communityName) -> initiateChallengeCmd fromPlayer toPlayer communityName |> Cmd.map ChallengeMessage
    | AcceptChallengeCmdMsg (notificationId, fromPlayer, communityName) -> acceptChallengeCmd notificationId fromPlayer communityName |> Cmd.map ChallengeMessage
    | DeclineChallengeCmdMsg (notificationId, toPlayer, communityName) -> declineChallengeCmd notificationId toPlayer communityName |> Cmd.map ChallengeMessage
    | CancelChallengeCmdMsg (toPlayer, communityName) -> cancelChallengeCmd toPlayer communityName |> Cmd.map ChallengeMessage
    | ReReadChallengesCmdMsg -> reReadChallengesCmd () |> Cmd.map ChallengeMessage

let toAddResultModel playerName opponentName communityName (resultModel: ResultModel): AddResultModel =
    { PlayerName = playerName
      OpponentName = opponentName
      CommunityName = communityName
      PlayerPoints = resultModel.OwnPoints
      OpponentPoints = resultModel.OpponentPoints
      ExtraTime = resultModel.ExtraTime }

let updateChallengeModel (ownName: string) (opponentName: string) (communityName: string) (challengeModel: ChallengeModel) (challengeMsg: ChallengeMsg) =
    match challengeMsg with
    | InitiateChallenge -> { challengeModel with IsProcessingChallenge = true }, [CmdMsg.InitiateChallengeCmdMsg (ownName, opponentName, communityName)]
    | AcceptChallenge notificationId -> { challengeModel with IsProcessingChallenge = true }, [CmdMsg.AcceptChallengeCmdMsg (notificationId, opponentName, communityName)]
    | DeclineChallenge notificationId -> { challengeModel with IsProcessingChallenge = true }, [CmdMsg.DeclineChallengeCmdMsg (notificationId, opponentName, communityName)]
    | CancelChallenge -> { challengeModel with IsProcessingChallenge = true }, [CmdMsg.CancelChallengeCmdMsg (opponentName, communityName)]
    | ChallengesUpdated newChallengeList -> { challengeModel with IsProcessingChallenge = false; Challenges = newChallengeList }, []
    | ChallengeAccepted -> challengeModel, [CmdMsg.ReReadChallengesCmdMsg]

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

let maxSelectablePoints = 20

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
                        command = (fun _ -> dispatch <| ChallengeMessage (AcceptChallenge notificationId))
                    ) |> applyImageButtonStyle
                    View.ImageButton(
                        source = Image.fromPath "decline_icon.png",
                        backgroundColor = Color.LightPink,
                        command = (fun _ -> dispatch <| ChallengeMessage (DeclineChallenge notificationId))
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
                                     challengeIcon (getChallengeState model.challengeModel.Challenges { PlayerName = opponentName; CommunityName = communityName }) model.challengeModel.IsProcessingChallenge dispatch
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
