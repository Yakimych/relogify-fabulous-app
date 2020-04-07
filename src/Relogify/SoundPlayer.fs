[<AutoOpen>]
module Relogify.SoundPlayer

open System.Reflection
open Plugin.SimpleAudioPlayer

type SoundType =
    | FinalSiren
    | ExpirationWarning
    | HalfTimeBeep

let getFileName (soundType: SoundType) =
    let nameWithoutExtension =
        match soundType with
        | FinalSiren -> "final_siren"
        | ExpirationWarning -> "expiration_warning"
        | HalfTimeBeep -> "half_time_beep"
    sprintf "%s.mp3" nameWithoutExtension

let getStreamFromFile (fileName: string) =
    let assembly = typedefof<SoundType>.GetTypeInfo().Assembly
    assembly.GetManifestResourceStream(sprintf "Relogify.sounds.%s" fileName)

let playSound (soundType: SoundType) =
    let fileName = getFileName soundType
    let stream = getStreamFromFile fileName
    CrossSimpleAudioPlayer.Current.Load(stream) |> ignore
    CrossSimpleAudioPlayer.Current.Play()

