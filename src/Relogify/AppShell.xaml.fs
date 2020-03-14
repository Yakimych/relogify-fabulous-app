namespace Relogify

open Xamarin.Forms.Xaml

type AppShell() =
    inherit Xamarin.Forms.Shell()
    let _ = base.LoadFromXaml(typeof<AppShell>)
