module Relogify.Timer

open System
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type TimerState =
    | NotRunning of int
    | Starting of int
    | Running of int
    | Pausing of int

type Model =
    { TotalTimeMs: int
      LastTickAt: DateTime
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
    | TimerTickCmdMsg -> tick 5000

let initModel =
     { TotalTimeMs = 60 * 1000
       LastTickAt = DateTime()
       ExtraTime = false
       State = NotRunning 0 }

let getTotalTime extraTime =
    if extraTime then 20 * 1000 else 60 * 1000

let update (model: Model) (msg: Msg): Model * CmdMsg list =
    match model.State, msg with
    | NotRunning timeElapsed, StartRequested -> { model with State = Starting timeElapsed }, [RequestStartCmdMsg]
    | NotRunning _, ToggleExtraTime ->
        let extraTime = not model.ExtraTime
        { model with State = NotRunning 0; TotalTimeMs = getTotalTime(extraTime) }, []
    | NotRunning _, Reset -> { model with State = NotRunning 0 }, []

    | Starting timeElapsed, Started startTime -> { model with State = Running timeElapsed; LastTickAt = startTime }, [TimerTickCmdMsg]

    | Running timeElapsed, PauseRequested -> { model with State = Pausing timeElapsed }, [RequestPauseCmdMsg]
    | Running _, Reset -> { model with State = NotRunning 0 }, []
    | Running timeElapsed, Tick timeOfTick ->
        let msElapsedSinceLastTick = int (timeOfTick - model.LastTickAt).TotalMilliseconds
        { model with State = Running (timeElapsed + msElapsedSinceLastTick); LastTickAt = timeOfTick }, [TimerTickCmdMsg]

    | Pausing timeElapsed, Paused pauseTime ->
        let msElapsedSinceLastTick = int (pauseTime - model.LastTickAt).TotalMilliseconds
        { model with State = NotRunning (timeElapsed + msElapsedSinceLastTick); LastTickAt = pauseTime }, []

    | _ -> model, []

let getTimeLeft (model: Model) =
    match model.State with
    | Running timeElapsed
    | Starting timeElapsed
    | Pausing timeElapsed
    | NotRunning timeElapsed -> model.TotalTimeMs - timeElapsed

let getState (model: Model) =
    match model.State with
    | Running timeElapsed -> sprintf "Running, timeElapsed: %d" timeElapsed
    | Starting timeElapsed -> sprintf "Starting... timeElapsed: %d" timeElapsed
    | Pausing timeElapsed -> sprintf "Pausing... timeElapsed: %d" timeElapsed
    | NotRunning timeElapsed -> sprintf "NotRunning, timeElapsed: %d" timeElapsed

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
