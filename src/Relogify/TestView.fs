module Relogify.TestView

open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { SomeText: string }

let init () = { SomeText = "Some text" }

let view model =
    View.StackLayout(
         padding = Thickness 20.0,
         verticalOptions = LayoutOptions.Center,
         children =
             [
                  View.Label(
                      text = sprintf "Some test view: %s" model.SomeText,
                      horizontalOptions = LayoutOptions.Center,
                      width = 200.0,
                      horizontalTextAlignment = TextAlignment.Center)
             ]
        )
