namespace Relogify.iOS

open Xamarin.Forms

type IosFirebaseMessagingService() =
    interface Relogify.IMessagingService with
        member this.SendRegistrationToServer(): Async<unit> = async { return () }
        member this.CancelNotification (_notificationId: int): unit = ()

[<assembly:Dependency(typeof<IosFirebaseMessagingService>)>]
do ()
