namespace Relogify.Android

open System
open Android.Util
open Firebase.Messaging
open Android.Support.V4.App
open WindowsAzure.Messaging
open Android.App
open Android.Content
open Xamarin.Forms

// TODO: Send challengeFrom and communityName as part of the message instead of parsing a string
module MessageUtils =
    let private parsePlayer (messagePlayerPart: string): string =
        let words = messagePlayerPart.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        match words |> List.ofArray with
        | [] -> failwith "The message player part cannot be empty"
        | playerName :: _otherWords -> playerName

    let parseCommunityAndPlayer (messageBody: string): (string * string) =
        let parts = messageBody.Split(":", StringSplitOptions.RemoveEmptyEntries)
        match parts |> List.ofArray with
        | [] -> failwith "Message cannot be empty!"
        | [_singleElement] -> failwith "Message should contain player information"
        | communityName :: secondPart :: _restOfMessage -> (communityName, secondPart |> parsePlayer)

type ResourceAlias = Resource

[<Service>]
[<IntentFilter([| "com.google.firebase.MESSAGING_EVENT" |])>]
[<IntentFilter([| "com.google.firebase.INSTANCE_ID_EVENT" |])>]
type MyFirebaseMessagingService() =
    inherit FirebaseMessagingService()

    let TAG: string = "MyFirebaseMsgService"
    let androidTokenPropertyKey: string = "android_notification_hub_token"

    interface Relogify.IMessagingService with
        member this.SendRegistrationToServer (): Async<unit> =
            async {
                let savedToken = Application.Current.Properties.[androidTokenPropertyKey] :?> string

                let listenConnectionString =
                    Relogify.ConfigManager.getListenConnectionString ()

                let notificationHubName =
                    Relogify.ConfigManager.getNotificationHubName ()

                let hub = new NotificationHub(notificationHubName, listenConnectionString, Android.App.Application.Context)

                let applicationSettings = Relogify.ApplicationSettings.getApplicationSettings ()
                let tags = applicationSettings.Communities |> List.map (fun c -> sprintf "%s_%s" c.CommunityName c.PlayerName)

                let regID = hub.Register(savedToken, tags |> Array.ofList).RegistrationId

                Log.Debug(TAG, sprintf "Successful registration of ID %s" regID) |> ignore
            }

    member private this.SendNotification(messageBody: string) =
        let intent =
            new Intent(this, typedefof<MainActivity>)

        intent.AddFlags(ActivityFlags.ClearTop) |> ignore

        let pendingIntent =
            PendingIntent.GetActivity(this, 0, intent, PendingIntentFlags.OneShot)

        let (communityName, challengeFrom) = messageBody |> MessageUtils.parseCommunityAndPlayer

        let acceptIntent = new Intent(this, typedefof<MyBroadcastReceiver>)
        acceptIntent.SetAction("ACTION_ACCEPT") |> ignore
        acceptIntent.PutExtra("EXTRA_NOTIFICATION_ID", 0) |> ignore
        acceptIntent.PutExtra("CHALLENGE_FROM", challengeFrom) |> ignore
        acceptIntent.PutExtra("COMMUNITY_NAME", communityName) |> ignore

        let acceptPendingIntent = PendingIntent.GetBroadcast(this, 0, acceptIntent, PendingIntentFlags.OneShot)

        let notificationBuilder =
            new NotificationCompat.Builder(this, MainActivity.CHANNEL_ID)

        notificationBuilder
            .SetSmallIcon(ResourceAlias.Drawable.ic_launcher)
            .SetContentText(messageBody)
            .SetAutoCancel(true)
            .SetShowWhen(false)
            .SetContentIntent(pendingIntent)
            .AddAction(ResourceAlias.Drawable.ic_launcher, "Accept", acceptPendingIntent)
        |> ignore

        let notificationManager = NotificationManager.FromContext(this)
        notificationManager.Notify(MainActivity.NOTIFICATION_ID, notificationBuilder.Build())

    override this.OnMessageReceived(message: RemoteMessage) =
        Log.Debug(TAG, sprintf "From: %s" message.From)
        |> ignore
        let notification = message.GetNotification()
        if notification <> null then
            Log.Debug(TAG, sprintf "Notification Message Body: %A" notification.Body)
            |> ignore
            this.SendNotification(notification.Body)
        else
            this.SendNotification(message.Data.Values |> Seq.head)
        ()

    override this.OnNewToken (token: string) =
        Log.Debug(TAG, sprintf "FCM token: %s" token)
        |> ignore

        async {
            Application.Current.Properties.[androidTokenPropertyKey] <- token
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask

            do! (this :> Relogify.IMessagingService).SendRegistrationToServer ()
        } |> Async.StartAsTask |> ignore

[<assembly: Dependency(typeof<MyFirebaseMessagingService>)>]
do ()