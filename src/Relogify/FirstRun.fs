module Relogify.FirstRun

open Fabulous.XamarinForms
open Relogify
open Relogify.Styles
open Xamarin.Forms
open Fabulous

type Model = PlayerSelector.Model
type Msg = PlayerSelector.Msg
type CmdMsg = PlayerSelector.CmdMsg
let mapCommands = PlayerSelector.mapCommands
let initModel = PlayerSelector.initModel

type OutMsg =
    | FirstRunComplete of communityName: string * playerName : string

let toOutMsg (msg: Msg): OutMsg option =
    match msg with
    | Msg.Confirm (communityName, playerName) -> FirstRunComplete (communityName, playerName) |> Some
    | _ -> None

let update (model: Model) (msg: Msg): Model * CmdMsg list * OutMsg option =
    let newModel, cmdMsgList = PlayerSelector.performTransition model msg
    newModel, cmdMsgList, toOutMsg msg

let view (model: Model) (dispatch: Msg -> unit) : ViewElement =
    View.NavigationPage(
        barBackgroundColor = navigationPrimaryColor,
        barTextColor = barTextColor,
        pages = [
            View.ContentPage(
                title = "Choose Community and Player",
                content = View.Grid(
                    margin = Thickness(10.0),
                    verticalOptions = LayoutOptions.FillAndExpand,
                    rowdefs = [ Star; Absolute 50.0; Auto ],
                    children = [
                        yield! PlayerSelector.playerSelectorView model dispatch
                    ]
                )
            )
        ]
    )
