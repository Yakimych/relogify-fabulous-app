namespace Relogify

open Fabulous.Core
open Fabulous.StaticView
open Xamarin.Forms

type Model =
  { Count : int
    Step : int }

type Msg =
    | Increment
    | Decrement
    | Reset
    | SetStep of int

type App () =
    inherit Application ()

    let init () = { Count = 0; Step = 3 }

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }
        | Decrement -> { model with Count = model.Count - model.Step }
        | Reset -> init ()
        | SetStep n -> { model with Step = n }

    let view () =
        AppShell (),
        [ ]

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