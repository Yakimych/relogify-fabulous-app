[<AutoOpen>]
module Relogify.SoundPlayer

open System.Reflection
open Plugin.SimpleAudioPlayer

type SomeType = Noop // TODO: Remove

let getStreamFromFile (fileName: string) =
    let assembly = typedefof<SomeType>.GetTypeInfo().Assembly
    assembly.GetManifestResourceStream(sprintf "Relogify.sounds.%s" fileName)

let playSound (fileName: string) =
    let stream = getStreamFromFile fileName
    CrossSimpleAudioPlayer.Current.Load(stream) |> ignore
    CrossSimpleAudioPlayer.Current.Play()

