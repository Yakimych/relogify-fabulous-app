namespace Relogify.iOS

open Xamarin.Forms

//[<Service>]
type IosFirebaseMessagingService() =
    interface Relogify.IMessagingService with
        member this.SendRegistrationToServer(): Async<unit> = async { return () }

[<assembly:Dependency(typeof<IosFirebaseMessagingService>)>]
do ()
