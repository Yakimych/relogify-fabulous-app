namespace Relogify

open Fabulous.Core
open Fabulous.StaticView
open Xamarin.Forms

type Model =
  { Count : int
    Step : int
    AboutPageModel : AboutPage.Model }

type Msg =
    | AboutPageMsg of AboutPage.Msg
    | Increment
    | Decrement
    | Reset
    | SetStep of int

type App () =
    inherit Application ()

    let init () = { Count = 0; Step = 3; AboutPageModel = AboutPage.init() }

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }
        | Decrement -> { model with Count = model.Count - model.Step }
        | Reset -> init ()
        | SetStep n -> { model with Step = n }
        | AboutPageMsg aboutPageMsg ->
            let newAboutPageModel = AboutPage.update aboutPageMsg model.AboutPageModel
            { model with AboutPageModel = newAboutPageModel }

    let view () =
        let _, aboutPageViewBinding = AboutPage.view ()
        let fromAboutPageMsg = fun (aboutPageMsg: AboutPage.Msg) -> aboutPageMsg |> Msg.AboutPageMsg

        AppShell (),
        [
            "AboutPageModel"
            |> Binding.subView
                (fun _ -> AboutPage.init () |> ignore)
                (fun m -> m.AboutPageModel)
                fromAboutPageMsg
                aboutPageViewBinding
        ]

    do
        let runner =
            Program.mkSimple init update view
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.runWithStaticView
        let page = runner.InitialMainPage

        do PlatformConfiguration.iOSSpecific.Page.SetUseSafeArea(page.On<PlatformConfiguration.iOS>(), true) |> ignore
        base.MainPage <- page