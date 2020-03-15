module Relogify.Timer

open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { TimeLeftMs: int
      closePage: unit -> unit }

let init closePage =
     { closePage = closePage
       TimeLeftMs = 60 * 1000 }

// TODO: How do we handle dispatch here?
let view model =
    View.NavigationPage(
        pages = [
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
                                          horizontalTextAlignment = TextAlignment.Center)
                                      View.Button(
                                          text = "Close",
                                          width = 200.0,
                                          command = model.closePage)
                                 ]
                    )
                ).ToolbarItems([View.ToolbarItem(text = "Close", command = model.closePage)])
            ])
