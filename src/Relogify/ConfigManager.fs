module Relogify.ConfigManager

open System.IO
open Newtonsoft.Json.Linq

// TODO: Remove
type TestType = Noop

let namespaceName = "Relogify"
let settingsFileName = "appsettings.json"

// TODO: Refactor this file
let getSettingsObject () =
    let assembly = typedefof<TestType>.Assembly

    use stream = assembly.GetManifestResourceStream(sprintf "%s.%s" namespaceName settingsFileName) // TODO: Clean up

    if isNull stream then
        failwith "stream is null"
    else
        let reader = new StreamReader(stream)
        let json = reader.ReadToEnd()
        let settingsObject = JObject.Parse(json)
        settingsObject

let getSettingStringValue (settingName: string) (settingsObject: JObject) =
    let node: JToken = settingsObject.[settingName]
    node.ToString()

let getApiUrl () =
    getSettingsObject() |> getSettingStringValue "ApiUrl"

let getListenConnectionString () =
    getSettingsObject() |> getSettingStringValue "ListenConnectionString"

let getNotificationHubName () =
    getSettingsObject() |> getSettingStringValue "NotificationHubName"

let getNotificationFunctionBaseUrl () =
    getSettingsObject() |> getSettingStringValue "NotificationFunctionBaseUrl"

let getChallengeFunctionCode () =
    getSettingsObject() |> getSettingStringValue "ChallengeFunctionCode"
