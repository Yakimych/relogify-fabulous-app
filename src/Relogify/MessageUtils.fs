module Relogify.MessageUtils

open System
open System.Text

// TODO: Unit tests/property tests with regex: "[a-zA-Z0-9-_.~%]+"
// https://firebase.google.com/docs/cloud-messaging/send-message#send_to_a_topic
let base64UrlEncode (plainText: string): string =
    Convert.ToBase64String(Encoding.UTF8.GetBytes(plainText)).TrimEnd('=').Replace('+', '-').Replace('/', '_');

let base64UrlDecode (base64EncodedData: string) =
    let incoming = base64EncodedData.Replace('_', '/').Replace('-', '+')
    let paddedString =
        match base64EncodedData.Length % 4 with
        | 2 -> incoming + "=="
        | 3 -> incoming + "="
        | _ -> incoming

    let bytes = Convert.FromBase64String(paddedString)
    Encoding.ASCII.GetString(bytes)

