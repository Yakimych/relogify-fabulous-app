[<AutoOpen>]
module Relogify.Helpers

open System

let stringToOption (stringValue: string) =
    if String.IsNullOrWhiteSpace(stringValue) then None else Some(stringValue)
