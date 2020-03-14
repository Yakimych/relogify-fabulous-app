namespace Relogify

open Xamarin.Forms
open Xamarin.Forms.Xaml

type SettingsPage() =
    inherit ContentPage()
    let _ = base.LoadFromXaml(typeof<SettingsPage>)
