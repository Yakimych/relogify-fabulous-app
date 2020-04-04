module Relogify.Timer

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type TimerState =
    | NotRunning of int
    | Running of int

type Model =
    { TotalTimeMs: int
      ExtraTime: bool
      State: TimerState }

type Msg =
    | Start
    | Pause
    | Reset
    | Tick of int
    | ToggleExtraTime

type CmdMsg =
    TimerTickCmdMsg

let tick (msBetweenTicks: int) =
    async { do! Async.Sleep msBetweenTicks
            return Tick msBetweenTicks }
    |> Cmd.ofAsyncMsg

let mapCommands =
    function
    | TimerTickCmdMsg -> tick 200

let initModel =
     { TotalTimeMs = 60 * 1000
       ExtraTime = false
       State = NotRunning 0 }

let transition (state: TimerState) (msg: Msg): TimerState =
    match state, msg with
    | NotRunning timeElapsed, Start -> Running timeElapsed
    | NotRunning _, ToggleExtraTime -> NotRunning 0
    | NotRunning _, Reset -> NotRunning 0

    | Running timeElapsed, Pause -> NotRunning timeElapsed
    | Running _, Reset -> NotRunning 0
    | Running timeElapsed, Tick timeBetweenTicks -> Running (timeElapsed + timeBetweenTicks)

    | _ -> state

let getTotalTime extraTime =
    if extraTime then 20 * 1000 else 60 * 1000

let update (model: Model) (msg: Msg): Model * CmdMsg list =
    let timerState = transition model.State msg
    let newModel = { model with State = timerState }

    match msg with
    | Start ->
        newModel, [TimerTickCmdMsg]
    | Tick _ ->
        newModel, [TimerTickCmdMsg]
    | ToggleExtraTime ->
        let extraTime = not model.ExtraTime
        { model with ExtraTime = extraTime; TotalTimeMs = getTotalTime(extraTime) }, []
    | _ -> newModel, []

let getTimeLeft (model: Model) =
    match model.State with
    | Running timeElapsed
    | NotRunning timeElapsed -> model.TotalTimeMs - timeElapsed

let getState (model: Model) =
    match model.State with
    | Running timeElapsed -> sprintf "Running, timeElapsed: %d" timeElapsed
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
                              command = (fun _ -> dispatch Start)
                          )
                          View.Button(
                              text = "Pause",
                              horizontalOptions = LayoutOptions.Center,
                              width = 200.0,
                              textColor = Color.White,
                              command = (fun _ -> dispatch Pause)
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
