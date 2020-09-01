module Relogify.Timer

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Xamarin.Essentials
open Relogify.SoundPlayer

type TimerState =
    | NotRunning
    | Starting
    | Running of lastTickAt: DateTime
    | Pausing of lastTickAt: DateTime

type Model =
    { TotalTimeMs: int
      TimeElapsedMs: int
      ExtraTime: bool
      HalfTimeBeepHasBeenPlayed: bool
      ExpirationWarningHasBeenPlayed: bool
      State: TimerState }

type Msg =
    | StartRequested
    | Started of DateTime
    | PauseRequested
    | Paused of DateTime
    | Reset
    | Tick of DateTime
    | ToggleExtraTime
    | SetSoundHasBeenPlayed of SoundType

type CmdMsg =
    | RequestStartCmdMsg
    | SetKeepScreenOnCmdMsg of bool
    | RequestPauseCmdMsg
    | TimerTickCmdMsg
    | PlaySoundCmdMsg of SoundType

type OutMsg =
    | TimerExpiredOutMsg of extraTime: bool

let tick (msBetweenTicks: int) =
    async {
        do! Async.Sleep msBetweenTicks
        return Tick DateTime.Now }
    |> Cmd.ofAsyncMsg

let requestStart () =
    Started DateTime.Now |> Cmd.ofMsg

let requestPlaySound (soundType: SoundType) =
    // TODO: The first time the sound is played results in a lag
    // Is there a way to pre-load the stream?
    async { playSound soundType } |> Async.StartImmediate
    SetSoundHasBeenPlayed soundType |> Cmd.ofMsg

let requestPause () = Paused DateTime.Now |> Cmd.ofMsg

let setKeepScreenOn (keepOn: bool) =
    DeviceDisplay.KeepScreenOn <- keepOn
    Cmd.none

let tickFrequencyMs = 200
let mapCommands =
    function
    | RequestStartCmdMsg -> requestStart ()
    | RequestPauseCmdMsg -> requestPause ()
    | PlaySoundCmdMsg soundType -> requestPlaySound soundType
    | TimerTickCmdMsg -> tick tickFrequencyMs
    | SetKeepScreenOnCmdMsg keepOn -> setKeepScreenOn keepOn

let normalTimeTotalMilliseconds = 5 * 60 * 1000
let extraTimeTotalMilliseconds = 2 * 60 * 1000
let expirationWarningMilliseconds = 30 * 1000

let getTotalTime extraTime =
    if extraTime then extraTimeTotalMilliseconds else normalTimeTotalMilliseconds

let isPastHalftTime (model: Model) =
    model.TimeElapsedMs > model.TotalTimeMs / 2

let isPastWarningTime (model: Model) =
    model.TimeElapsedMs > model.TotalTimeMs - expirationWarningMilliseconds

let initModel =
     let extraTime = false
     { TotalTimeMs = extraTime |> getTotalTime
       TimeElapsedMs = 0
       ExtraTime = extraTime
       HalfTimeBeepHasBeenPlayed = false
       ExpirationWarningHasBeenPlayed = false
       State = NotRunning }

let getSoundToPlayCmdMsgs (model: Model): CmdMsg list =
    if isPastHalftTime model && not model.HalfTimeBeepHasBeenPlayed then
        [PlaySoundCmdMsg HalfTimeBeep]
    else if isPastWarningTime model && not model.ExpirationWarningHasBeenPlayed then
        [PlaySoundCmdMsg ExpirationWarning]
    else
        []

let update (model: Model) (msg: Msg): Model * CmdMsg list * OutMsg option =
    match model.State, msg with
    | NotRunning, StartRequested -> { model with State = Starting }, [RequestStartCmdMsg], None
    | NotRunning _, ToggleExtraTime ->
        let extraTime = not model.ExtraTime
        { model with State = NotRunning; ExtraTime = extraTime; TotalTimeMs = getTotalTime(extraTime); TimeElapsedMs = 0 }, [], None
    | NotRunning _, Reset -> { model with State = NotRunning; TimeElapsedMs = 0; HalfTimeBeepHasBeenPlayed = false; ExpirationWarningHasBeenPlayed = false }, [], None

    | Starting, Started startTime -> { model with State = Running startTime }, [TimerTickCmdMsg; SetKeepScreenOnCmdMsg true], None

    | Running lastTickAt, PauseRequested -> { model with State = Pausing lastTickAt }, [RequestPauseCmdMsg], None
    | Running _, Reset -> { model with State = NotRunning; TimeElapsedMs = 0 }, [SetKeepScreenOnCmdMsg false], None
    | Running lastTickAt, Tick newTickAt ->
        let msElapsedSinceLastTick = int (newTickAt - lastTickAt).TotalMilliseconds
        let newTimeElapsed = (model.TimeElapsedMs + msElapsedSinceLastTick)

        if newTimeElapsed >= model.TotalTimeMs then
            model, [PlaySoundCmdMsg FinalSiren; SetKeepScreenOnCmdMsg false], Some (TimerExpiredOutMsg model.ExtraTime)
        else
            { model with State = Running newTickAt; TimeElapsedMs = newTimeElapsed }, [TimerTickCmdMsg] @ getSoundToPlayCmdMsgs model, None
    | Running _, SetSoundHasBeenPlayed soundType ->
        let newModel =
            match soundType with
            | HalfTimeBeep -> { model with HalfTimeBeepHasBeenPlayed = true }
            | ExpirationWarning -> { model with ExpirationWarningHasBeenPlayed = true }
            | FinalSiren -> model

        newModel, [], None

    | Pausing lastTickAt, Paused pauseTime ->
        let msElapsedSinceLastTick = int (pauseTime - lastTickAt).TotalMilliseconds
        { model with State = NotRunning; TimeElapsedMs = (model.TimeElapsedMs + msElapsedSinceLastTick) }, [SetKeepScreenOnCmdMsg false], None

    | _ -> model, [], None

let getFormattedTimeLeft (numberOfMilliseconds: int): string =
    let totalSecondsLeft = Math.Ceiling((decimal numberOfMilliseconds) / 1000m)
    let minutesLeft = totalSecondsLeft / 60m
    let secondsLeftInCurrentMinute = int totalSecondsLeft % 60

    sprintf "%02i:%02i" (int minutesLeft) secondsLeftInCurrentMinute

let canToggleExtraTime (state: TimerState): bool =
    match state with
    | NotRunning -> true
    | _ -> false

let canStartTimer (state: TimerState): bool =
    match state with
    | NotRunning -> true
    | _ -> false

let canStopTimer (state: TimerState): bool =
    match state with
    | Running _ -> true
    | _ -> false

let applyTimerButtonStyle (button: ViewElement) =
    button
        .Margin(Thickness(15.0))
        .ButtonCornerRadius(10)
        .BorderWidth(2.0)
        .Height(60.0)
        .FontSize(FontSize.fromValue(36.0))

let view (model: Model) dispatch =
    View.ContentPage(
        title = "Match Timer",
        backgroundColor = Color.Black,
        content =
            View.Grid(
                rowdefs = [ Auto; Absolute 120.0; Absolute 120.0; Star ],
                children =
                    [
                        View.Grid(
                            coldefs = [ Star; Auto; Star ],
                            children = [
                                View.Grid(
                                    horizontalOptions = LayoutOptions.CenterAndExpand,
                                    verticalOptions = LayoutOptions.Center,
                                    margin = Thickness(15.0),
                                    height = 200.0,
                                    children = [
                                        View.Label(
                                            text = getFormattedTimeLeft (model.TotalTimeMs - model.TimeElapsedMs),
                                            horizontalOptions = LayoutOptions.CenterAndExpand,
                                            verticalOptions = LayoutOptions.Center,
                                            margin = Thickness(15.0),
                                            textColor = Color.White,
                                            fontSize = FontSize.fromValue(72.0)
                                        )
                                    ]
                                ).Column(1)

                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    verticalOptions = LayoutOptions.Center,
                                    horizontalOptions= LayoutOptions.Center,
                                    children = [
                                        View.Switch(
                                            isToggled = model.ExtraTime,
                                            isEnabled = canToggleExtraTime model.State,
                                            onColor = Color.Orange,
                                            toggled = (fun _ -> dispatch ToggleExtraTime)
                                        )
                                        View.Label(
                                            fontSize = FontSize.fromNamedSize(NamedSize.Small),
                                            text = sprintf "Extra%sTime" Environment.NewLine,
                                            textColor = Color.White
                                        )
                                    ]
                                ).Column(2)
                            ]
                        ).Row(0)

                        View.Button(
                            text = "Start",
                            isVisible = canStartTimer model.State,
                            backgroundColor = Color.DarkGreen,
                            borderColor = Color.White,
                            textColor = Color.Yellow,
                            command = (fun _ -> dispatch StartRequested)
                        ).Row(1) |> applyTimerButtonStyle
                        View.Button(
                            text = "Pause",
                            isVisible = canStopTimer model.State,
                            backgroundColor = Color.LightGray,
                            borderColor = Color.White,
                            textColor = Color.Black,
                            command = (fun _ -> dispatch PauseRequested)
                        ).Row(1) |> applyTimerButtonStyle
                        View.Button(
                            text = "Reset",
                            isVisible = true,
                            backgroundColor = Color.Red,
                            borderColor = Color.White,
                            textColor = Color.Black,
                            command = (fun _ -> dispatch Reset)
                        ).Row(2) |> applyTimerButtonStyle
                    ]
            )
        )
