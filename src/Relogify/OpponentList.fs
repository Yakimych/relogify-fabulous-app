module Relogify.OpponentList

open Fabulous.XamarinForms
open Xamarin.Forms

type Model =
    { OpponentNames: string list }

let initModel = { OpponentNames = [ "Kalle"; "Leif" ] }

let view model =
    View.ContentPage(
        title = "Select Opponent",
        content = View.CollectionView(
            items = [
                View.StackLayout
                    (padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
                     children =
                         [ View.Label
                             (text = "Placeholder", horizontalOptions = LayoutOptions.Center, width = 200.0,
                              horizontalTextAlignment = TextAlignment.Center) ])
                    ]
            )
        )
