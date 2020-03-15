//namespace Relogify

module Relogify.Routes

open Fabulous
open Xamarin.Forms

[<QueryProperty("Name", "name")>]
type RoutingPage(view: unit -> ViewElement) =
    inherit ContentPage()

    let mutable _name = ""
    let mutable _prevViewElement = None

    member this.Name
        with get () = _name
        and set (value: string) =
            _name <- value.Replace("%20", " ")
            this.Refresh()

    member this.Refresh() =
        let viewElement = view()
        match _prevViewElement with
        | None -> this.Content <- viewElement.Create() :?> View
        | Some prevViewElement -> viewElement.UpdateIncremental(prevViewElement, this.Content)

type TestRoutingPage() =
    inherit RoutingPage(TestView.init >> TestView.view)
