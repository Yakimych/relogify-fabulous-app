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

let initModel =
     { TotalTimeMs = 60 * 1000
       TimeElapsedMs = 0
       ExtraTime = false
       State = NotRunning }

let getTotalTime extraTime =
    if extraTime then 20 * 1000 else 60 * 1000

let update (model: Model) (msg: Msg): Model * CmdMsg list =
    match model.State, msg with
    | NotRunning, StartRequested -> { model with State = Starting }, [RequestStartCmdMsg]
    | NotRunning _, ToggleExtraTime ->
        let extraTime = not model.ExtraTime
        { model with State = NotRunning; TotalTimeMs = getTotalTime(extraTime) }, []
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

let getTimeLeft (model: Model) =
    model.TotalTimeMs - model.TimeElapsedMs

// TODO: Remove
let getState (model: Model) =
    match model.State with
    | Running _ -> sprintf "Running, timeElapsed: %d" model.TimeElapsedMs
    | Starting -> sprintf "Starting... timeElapsed: %d" model.TimeElapsedMs
    | Pausing _ -> sprintf "Pausing... timeElapsed: %d" model.TimeElapsedMs
    | NotRunning -> sprintf "NotRunning, timeElapsed: %d" model.TimeElapsedMs

let view model dispatch =
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
                                            text = "05:00",
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
                                            isToggled = false, // TODO: ExtraTime
                                            isEnabled = false, // TODO
                                            onColor = Color.Orange,
                                            toggled = (fun _ -> dispatch ToggleExtraTime)
                                        )
                                        View.Label(
                                            fontSize = FontSize.Named(NamedSize.Small),
                                            text = "Extra Time", // TODO: nbsp
                                            textColor = Color.White
                                        )
                                    ]
                                ).Column(2)
                            ]
                        ).Row(0)

                        View.Button(
                            text = "Start",
                            // TODO: withButtonStyle?
                            margin = Thickness(15.0),
                            cornerRadius = 10,
                            borderWidth = 2.0,
                            height = 60.0,
                            fontSize = FontSize.FontSize(36.0),
                            isVisible = true, // TODO
                            backgroundColor = Color.DarkGreen,
                            borderColor = Color.White,
                            textColor = Color.Yellow,
                            command = (fun _ -> dispatch StartRequested)
                        ).Row(1)
                        View.Button(
                            text = "Pause",
                            // TODO: withButtonStyle?
                            margin = Thickness(15.0),
                            cornerRadius = 10,
                            borderWidth = 2.0,
                            height = 60.0,
                            fontSize = FontSize.FontSize(36.0),
                            isVisible = false, // TODO
                            backgroundColor = Color.LightGray,
                            borderColor = Color.White,
                            textColor = Color.Black,
                            command = (fun _ -> dispatch PauseRequested)
                        ).Row(1)
                        View.Button(
                            text = "Reset",
                            // TODO: withButtonStyle?
                            margin = Thickness(15.0),
                            cornerRadius = 10,
                            borderWidth = 2.0,
                            height = 60.0,
                            fontSize = FontSize.FontSize(36.0),
                            isVisible = true, // TODO
                            backgroundColor = Color.Red,
                            borderColor = Color.White,
                            textColor = Color.Black,
                            command = (fun _ -> dispatch Reset)
                        ).Row(2)
                    ]
            )
        )
