module Relogify.Timer

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { TimeLeftMs: int } // TODO

type Msg =
    | Start
    // TODO

let initModel =
     { TimeLeftMs = 60 * 1000 }

let update (model: Model) (msg: Msg): Model * Cmd<Msg> =
    match msg with
    | Start -> { model with TimeLeftMs = 0 }, Cmd.none

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
                              text = sprintf "Time left: %d" model.TimeLeftMs,
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
                     ]
        )
    )
