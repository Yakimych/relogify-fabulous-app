module Relogify.About

open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { AboutText: string }

let initModel = { AboutText = "About" }

let view model =
    View.ContentPage(
        title = "About",
        icon = ImagePath "tab_about.png",
        content = View.CollectionView(
            items = [
                View.StackLayout
                    (padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
                     children =
                         [ View.Label
                             (text = model.AboutText, horizontalOptions = LayoutOptions.Center, width = 200.0,
                              horizontalTextAlignment = TextAlignment.Center) ])
                    ]
            )
        )
