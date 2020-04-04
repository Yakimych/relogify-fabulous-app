module Relogify.Timer

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type TimerState =
    | NotRunning
    | Starting
    | Running of lastTickAt: DateTime
    | Pausing of lastTickAt: DateTime

type Model =
    { TotalTimeMs: int
      TimeElapsedMs: int
      ExtraTime: bool
      State: TimerState }

type Msg =
    | StartRequested
    | Started of DateTime
    | PauseRequested
    | Paused of DateTime
    | Reset
    | Tick of DateTime
    | ToggleExtraTime

type CmdMsg =
    | RequestStartCmdMsg
    | RequestPauseCmdMsg
    | TimerTickCmdMsg

let tick (msBetweenTicks: int) =
    async {
        do! Async.Sleep msBetweenTicks
        return Tick DateTime.Now }
    |> Cmd.ofAsyncMsg

let requestStart () = Started DateTime.Now |> Cmd.ofMsg
let requestPause () = Paused DateTime.Now |> Cmd.ofMsg

let mapCommands =
    function
    | RequestStartCmdMsg -> requestStart ()
    | RequestPauseCmdMsg -> requestPause ()
    | TimerTickCmdMsg -> tick 200

let normalTimeTotalMilliseconds = 5 * 60 * 1000
let extraTimeTotalMilliseconds = 2 * 60 * 1000

let getTotalTime extraTime =
    if extraTime then extraTimeTotalMilliseconds else normalTimeTotalMilliseconds

let initModel () =
     let extraTime = false
     { TotalTimeMs = extraTime |> getTotalTime
       TimeElapsedMs = 0
       ExtraTime = extraTime
       State = NotRunning }

let update (model: Model) (msg: Msg): Model * CmdMsg list =
    match model.State, msg with
    | NotRunning, StartRequested -> { model with State = Starting }, [RequestStartCmdMsg]
    | NotRunning _, ToggleExtraTime ->
        let extraTime = not model.ExtraTime
        { model with State = NotRunning; ExtraTime = extraTime; TotalTimeMs = getTotalTime(extraTime); TimeElapsedMs = 0 }, []
    | NotRunning _, Reset -> { model with State = NotRunning; TimeElapsedMs = 0 }, []

    | Starting, Started startTime -> { model with State = Running startTime }, [TimerTickCmdMsg]

    | Running lastTickAt, PauseRequested -> { model with State = Pausing lastTickAt }, [RequestPauseCmdMsg]
    | Running _, Reset -> { model with State = NotRunning; TimeElapsedMs = 0 }, []
    | Running lastTickAt, Tick newTickAt ->
        let msElapsedSinceLastTick = int (newTickAt - lastTickAt).TotalMilliseconds
        { model with State = Running newTickAt; TimeElapsedMs = (model.TimeElapsedMs + msElapsedSinceLastTick) }, [TimerTickCmdMsg]

    | Pausing lastTickAt, Paused pauseTime ->
        let msElapsedSinceLastTick = int (pauseTime - lastTickAt).TotalMilliseconds
        { model with State = NotRunning; TimeElapsedMs = (model.TimeElapsedMs + msElapsedSinceLastTick) }, []

    | _ -> model, []

let getFormattedTimeLeft (model: Model): string =
    let timeLeftMs = model.TotalTimeMs - model.TimeElapsedMs
    let totalSecondsLeft = timeLeftMs / 1000
    let minutesLeft = totalSecondsLeft / 60
    let secondsLeftInCurrentMinute = totalSecondsLeft % 60

    sprintf "%02i:%02i" minutesLeft secondsLeftInCurrentMinute

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
        .FontSize(FontSize.FontSize(36.0))

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
                                            text = getFormattedTimeLeft model,
                                            horizontalOptions = LayoutOptions.CenterAndExpand,
                                            verticalOptions = LayoutOptions.Center,
                                            margin = Thickness(15.0),
                                            textColor = Color.White,
                                            fontSize = FontSize.FontSize(72.0)
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
                                            fontSize = FontSize.Named(NamedSize.Small),
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
