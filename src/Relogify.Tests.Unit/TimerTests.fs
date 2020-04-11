module TimerTests

open System
open Relogify
open Xunit
open Timer

[<Fact>]
let ``Timer is stopped initially`` () =
    Assert.Equal(NotRunning, Timer.initModel.State)

[<Fact>]
let ``Extra time is toggled while timer is not running`` () =
    let model = Timer.initModel
    let modelWithToggledExtraTime, _, _ = update model ToggleExtraTime

    Assert.Equal(true, modelWithToggledExtraTime.ExtraTime)

let modelReducer (model: Model) (msg: Msg) =
    let newModel, _, _ = update model msg
    newModel

[<Fact>]
let ``Extra time is not toggled while timer is running`` () =
    let model = Timer.initModel
    let startTime = DateTime(2000, 01, 01)
    let actions = [StartRequested; Started startTime; ToggleExtraTime]

    let finalModel = actions |> List.fold modelReducer model

    Assert.Equal(false, finalModel.ExtraTime)

[<Fact>]
let ``Elapsed time does not get incremented white timer is paused`` () =
    let model = Timer.initModel
    let startTime = DateTime(2000, 01, 01, 10, 00, 00)
    let millisecondsBetweenTicks = 1000.0
    let millisecondsBetweenTickAndPause = 300.0
    let timeOfFirstTick = startTime.AddMilliseconds(millisecondsBetweenTicks)

    // Pause between ticks
    let timeOfPause = timeOfFirstTick.AddMilliseconds(millisecondsBetweenTickAndPause)
    let startTimeAfterPause = timeOfPause.AddMinutes(10.0)
    let timeOfSecondTick = startTimeAfterPause.AddMilliseconds(millisecondsBetweenTicks)

    let actions =
        [ StartRequested
          Started startTime
          Tick timeOfFirstTick
          PauseRequested
          Paused timeOfPause
          StartRequested
          Started startTimeAfterPause
          Tick timeOfSecondTick ]

    let finalModel = actions |> List.fold modelReducer model

    let expectedTimeElapsed = int <| millisecondsBetweenTicks * 2.0 + millisecondsBetweenTickAndPause
    Assert.Equal(expectedTimeElapsed, finalModel.TimeElapsedMs)

[<Theory>]
[<InlineData(300_000, "05:00")>]
[<InlineData(299_999, "05:00")>]
[<InlineData(299_001, "05:00")>]
[<InlineData(299_000, "04:59")>]
[<InlineData(60_001, "01:01")>]
[<InlineData(60_000, "01:00")>]
[<InlineData(59_999, "01:00")>]
[<InlineData(59_001, "01:00")>]
[<InlineData(59_000, "00:59")>]
[<InlineData(10_000, "00:10")>]
[<InlineData(05_000, "00:05")>]
[<InlineData(01_000, "00:01")>]
[<InlineData(00_999, "00:01")>]
[<InlineData(00_500, "00:01")>]
[<InlineData(00_001, "00:01")>]
[<InlineData(00_000, "00:00")>]
let ``Remaining time is displayed correctly`` (timeRemainingMs: int) (expectedFormattedString: string) =
    let formattedTime = Timer.getFormattedTimeLeft timeRemainingMs
    Assert.Equal(expectedFormattedString, formattedTime)

