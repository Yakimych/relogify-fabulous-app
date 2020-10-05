module AddResultTests

open Relogify
open Xunit
open AddResult
open FsCheck.Xunit

let initialModel = initModel ()

let updateModel (model: Model) (msg: Msg): Model =
    let updateModel, _ = update model msg "playerName" "opponentName" "communityName"
    updateModel

[<Property>]
let ``Own points are updated after dispatching SetOwnPoints`` (newOwnPoints: int) =
    let newModel = updateModel initialModel (SetOwnPoints newOwnPoints)

    let expectedResultModel = { initialModel.resultModel with OwnPoints = newOwnPoints }
    Assert.Equal({ initialModel with resultModel = expectedResultModel }, newModel)

[<Property>]
let ``Opponents points are updated after dispatching SetOpponentPoints`` (newOpponentPoints: int) =
    let newModel = updateModel initialModel (SetOpponentPoints newOpponentPoints)

    let expectedResultModel = { initialModel.resultModel with OpponentPoints = newOpponentPoints }
    Assert.Equal({ initialModel with resultModel = expectedResultModel }, newModel)

[<Property>]
let ``Result cannot be edited while adding result`` (oldOwnPoints: int) (oldOpponentPoints: int) =
    let newOwnPoints = oldOwnPoints + 1
    let newOpponentPoints = oldOpponentPoints + 1

    let actionsBeforeInitiatingAddResult = [
        SetOwnPoints oldOwnPoints
        SetOpponentPoints oldOpponentPoints
        ToggleExtraTime
    ]
    let modelBeforeInitiatingAddResult = actionsBeforeInitiatingAddResult |> List.fold updateModel initialModel
    let oldExtraTime = modelBeforeInitiatingAddResult.resultModel.ExtraTime

    let actionsAfterInitiatingAddResult = [
        AddResultInitiated
        SetOwnPoints newOwnPoints
        SetOpponentPoints newOpponentPoints
        ToggleExtraTime
    ]

    let finalModel = actionsAfterInitiatingAddResult |> List.fold updateModel modelBeforeInitiatingAddResult

    Assert.Equal(finalModel.resultModel.OwnPoints, oldOwnPoints)
    Assert.Equal(finalModel.resultModel.OpponentPoints, oldOpponentPoints)
    Assert.Equal(finalModel.resultModel.ExtraTime, oldExtraTime)

