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

// TODO: Add tests for Tick

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

