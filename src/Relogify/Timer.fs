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
            View.StackLayout(
                 padding = Thickness 20.0,
                 verticalOptions = LayoutOptions.Center,
                 children =
                     [
                          View.Label(
                              text = sprintf "State: %s" (model |> getState),
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              horizontalTextAlignment = TextAlignment.Center
                          )
                          View.Label(
                              text = sprintf "Time left: %d" (model |> getTimeLeft),
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              horizontalTextAlignment = TextAlignment.Center
                          )
                          View.Button(
                              text = "Start",
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              command = (fun _ -> dispatch StartRequested)
                          )
                          View.Button(
                              text = "Pause",
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              command = (fun _ -> dispatch PauseRequested)
                          )
                          View.Button(
                              text = "Reset",
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              command = (fun _ -> dispatch Reset)
                          )
                     ]
        )
    )
