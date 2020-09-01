module Relogify.About

open System.Reflection
open Fabulous.XamarinForms
open Xamarin.Forms
open Xamarin.Essentials

[<Literal>]
let relogifyUrl = "https://relogify.com"

let openBrowser (url: string) = fun () -> Browser.OpenAsync(url) |> ignore

let openBrowserOnTap (url: string) = View.TapGestureRecognizer(command = openBrowser url)

let openRelogifyInBrowser () = openBrowserOnTap relogifyUrl

let aboutRelogify () =
    let image = ImageSource.FromResource("Relogify.logo.png", Assembly.GetExecutingAssembly())
    let imageSource = Image.fromImageSource image

    View.StackLayout(
        margin = Thickness(20.0),
        children = [
            View.Label(
                text = "Relogify",
                margin = Thickness(0., 0., 0., 10.0),
                fontAttributes = FontAttributes.Bold,
                fontSize = FontSize.fromNamedSize(NamedSize.Large),
                horizontalOptions = LayoutOptions.Center)

            View.Label(text = "Save all match results, analyze, view head-to-head records, statistics and more.")

            View.Label(text = "Feature list", fontAttributes = FontAttributes.Bold, margin = Thickness(0., 20., 0., 0.))

            View.StackLayout(
                children = [
                    View.Label(text = "- Weekly Results and Leaderboards", fontSize = FontSize.fromNamedSize(NamedSize.Small))
                    View.Label(text = "- Head-to-Head results and statistics", fontSize = FontSize.fromNamedSize(NamedSize.Small))
                    View.Label(text = "- Individual player results, stats and winning streaks", fontSize = FontSize.fromNamedSize(NamedSize.Small))
                    View.Label(text = "- All-time history and Elo ratings", fontSize = FontSize.fromNamedSize(NamedSize.Small))
                ])

            View.Label(
               text = "Click here to learn more",
               fontAttributes = FontAttributes.Bold,
               textColor = Color.Blue,
               textDecorations = TextDecorations.Underline,
               gestureRecognizers = [ openRelogifyInBrowser() ]
            )
            View.Image(source = imageSource, gestureRecognizers = [ openRelogifyInBrowser() ])
        ]
    )

let view () =
    View.ContentPage(
        title = "About",
        icon = Image.fromPath "tab_about.png",
        content = aboutRelogify ()
    )
