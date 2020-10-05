module TimerTests

open System
open Relogify
open Xunit
open Timer
open FsCheck.Xunit

[<Fact>]
let ``Timer is stopped initially``() = Assert.Equal(NotRunning, initModel.State)

[<Fact>]
let ``Extra time is toggled while timer is not running``() =
    let model = initModel
    let modelWithToggledExtraTime, _, _ = update model ToggleExtraTime

    Assert.Equal(true, modelWithToggledExtraTime.ExtraTime)

let modelReducer (model: Model) (msg: Msg) =
    let newModel, _, _ = update model msg
    newModel

[<Fact>]
let ``Extra time is not toggled while timer is running``() =
    let model = initModel
    let startTime = DateTime(2000, 01, 01)

    let actions =
        [ StartRequested
          Started startTime
          ToggleExtraTime ]

    let finalModel = actions |> List.fold modelReducer model

    Assert.Equal(false, finalModel.ExtraTime)

[<Property>]
let ``Elapsed time does not get incremented while timer is paused``
    (startTime: DateTime)
    (millisecondsBetweenTicks: int)
    (millisecondsBetweenTickAndPause: int)
    =
    let model = initModel
    let timeOfFirstTick = startTime.AddMilliseconds(float millisecondsBetweenTicks)

    // Pause between ticks
    let timeOfPause = timeOfFirstTick.AddMilliseconds(float millisecondsBetweenTickAndPause)
    let startTimeAfterPause = timeOfPause.AddMinutes(10.0)
    let timeOfSecondTick = startTimeAfterPause.AddMilliseconds(float millisecondsBetweenTicks)

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

    let expectedTimeElapsed = millisecondsBetweenTicks * 2 + millisecondsBetweenTickAndPause
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
    let formattedTime = getFormattedTimeLeft timeRemainingMs
    Assert.Equal(expectedFormattedString, formattedTime)
