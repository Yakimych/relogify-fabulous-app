namespace Relogify

open Fabulous.Core
open Fabulous.StaticView
open Xamarin.Forms

type Model =
  { Count : int
    Step : int
    TabBarIsVisible : bool
    AboutPageModel : AboutPage.Model
    MatchTimerModel : MatchTimer.Model }

type Msg =
    | AboutPageMsg of AboutPage.Msg
    | MatchTimerMsg of MatchTimer.Msg
    | Increment
    | Decrement
    | Reset
    | SetStep of int

type App () =
    inherit Application ()

    let init () = { Count = 0; Step = 3; TabBarIsVisible = false; AboutPageModel = AboutPage.init(); MatchTimerModel = MatchTimer.init() }

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }
        | Decrement -> { model with Count = model.Count - model.Step }
        | Reset -> init ()
        | SetStep n -> { model with Step = n }
        | AboutPageMsg aboutPageMsg ->
            let newAboutPageModel = AboutPage.update aboutPageMsg model.AboutPageModel
            { model with AboutPageModel = newAboutPageModel }
        | MatchTimerMsg matchTimerMsg ->
            let newMatchTimerModel = MatchTimer.update matchTimerMsg model.MatchTimerModel
            { model with MatchTimerModel = newMatchTimerModel }

    let view () =
        let _, aboutPageViewBinding = AboutPage.view ()
        let fromAboutPageMsg = fun (aboutPageMsg: AboutPage.Msg) -> aboutPageMsg |> Msg.AboutPageMsg

        let _, matchTimerViewBinding = MatchTimer.view ()
        let fromMatchTimerMsg = fun (matchTimerMsg: MatchTimer.Msg) -> matchTimerMsg |> Msg.MatchTimerMsg

        AppShell (),
        [
            "TabBarIsVisible" |> Binding.oneWay (fun m -> m.TabBarIsVisible)
            "AboutPageModel"
            |> Binding.subView
                (fun _ -> AboutPage.init () |> ignore)
                (fun m -> m.AboutPageModel)
                fromAboutPageMsg
                aboutPageViewBinding
            "MatchTimerModel"
            |> Binding.subView
                (fun _ -> MatchTimer.init () |> ignore)
                (fun m -> m.MatchTimerModel)
                fromMatchTimerMsg
                matchTimerViewBinding
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