namespace Relogify

open System
open Xamarin.Forms
open Xamarin.Forms.Xaml
open Fabulous.StaticView

type MatchTimer() =
    inherit ContentPage()
    let _ = base.LoadFromXaml(typeof<MatchTimer>)

    member this.HandleCloseClicked (sender: Object, e: EventArgs): unit =
        this.Navigation.PopModalAsync() |> ignore

module MatchTimer =
    type Model =
        { TimeLeft: string }

    type Msg =
        | SetTimeLeft

    let init () = { TimeLeft = "5:00" }

    let update msg model =
        match msg with
        | SetTimeLeft -> { model with TimeLeft = "0!" }

    let view () =
        MatchTimer(),
        [
            "TimeLeft" |> Binding.oneWay (fun m -> m.TimeLeft)
            "SetTimeLeft" |> Binding.msg SetTimeLeft
        ]