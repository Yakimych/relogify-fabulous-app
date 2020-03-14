namespace Relogify

open Xamarin.Forms
open Xamarin.Forms.Xaml
open Fabulous.StaticView

open System

type AboutPage() =
    inherit ContentPage()
    let _ = base.LoadFromXaml(typeof<AboutPage>)

    member this.HandleOpenTimerClicked (sender: Object, e: EventArgs): unit =
        this.Navigation.PushModalAsync(new NavigationPage(new MatchTimer())) |> ignore

module AboutPage =
    type Model =
        { SomeText: string }

    type Msg =
        | SetSomeText

    let init () = { SomeText = "asd" }

    let update msg model =
        match msg with
        | SetSomeText -> { model with SomeText = "New Text" }

    // TODO: Does it make sense to have the view, or is it enough with just the ViewBindings here?
    let view () =
        AboutPage(),
        [
            "SomeText" |> Binding.oneWay (fun m -> m.SomeText)
            "SetSomeText" |> Binding.msg SetSomeText
        ]