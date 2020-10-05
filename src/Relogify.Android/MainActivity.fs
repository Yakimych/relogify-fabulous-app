// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace Relogify.Android

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.Runtime
open Android.Views
open Android.Widget
open Android.OS
open Xamarin.Forms.Platform.Android

open Android.Util
open Android.Gms.Common

[<Activity (Label = "Relogify.Android", Icon = "@mipmap/icon", Theme = "@style/MainTheme", MainLauncher = true, ConfigurationChanges = (ConfigChanges.ScreenSize ||| ConfigChanges.Orientation), ScreenOrientation = ScreenOrientation.Portrait)>]
type MainActivity() =
    inherit FormsAppCompatActivity()

    let TAG: string = "MainActivity"
    static member CHANNEL_ID: string = "relogify_challenges_channel"

    member private this.CreateNotificationChannel () =
        if Build.VERSION.SdkInt >= BuildVersionCodes.O then
            let channelName = "Challenge notifications"
            let channelDescription = "Notifications when receiving challenges from other players"
            let channel = new NotificationChannel(MainActivity.CHANNEL_ID, channelName, NotificationImportance.Default)
            channel.Description <- channelDescription

            let notificationManager = this.GetSystemService(Context.NotificationService) :?> NotificationManager
            notificationManager.CreateNotificationChannel(channel)
        else
            ()

    member this.IsPlayServicesAvailable () =
        let resultCode = GoogleApiAvailability.Instance.IsGooglePlayServicesAvailable(this)
        if resultCode <> ConnectionResult.Success then
            if GoogleApiAvailability.Instance.IsUserResolvableError(resultCode) then
                Log.Debug(TAG, GoogleApiAvailability.Instance.GetErrorString(resultCode)) |> ignore
            else
                Log.Debug(TAG, "This device is not supported") |> ignore
                this.Finish() |> ignore
            false
        else
            Log.Debug(TAG, "Google Play Services is available.") |> ignore
            true


    override this.OnCreate (bundle: Bundle) =
        FormsAppCompatActivity.TabLayoutResource <- Resources.Layout.Tabbar
        FormsAppCompatActivity.ToolbarResource <- Resources.Layout.Toolbar
        base.OnCreate (bundle)

        if this.Intent.Extras <> null then
            this.Intent.Extras.KeySet()
            |> Seq.iter (fun key ->
                if key <> null then
                    let value = this.Intent.Extras.GetString(key)
                    Log.Debug(TAG, sprintf "Key: '%s'; Value: '%s'." key value) |> ignore
                else
                    ()
            )
        else
            ()

        this.IsPlayServicesAvailable() |> ignore
        this.CreateNotificationChannel()

        Xamarin.Essentials.Platform.Init(this, bundle)

        Xamarin.Forms.Forms.Init (this, bundle)

        let appcore  = new Relogify.App()
        this.LoadApplication (appcore)

    override this.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<GeneratedEnum>] grantResults: Android.Content.PM.Permission[]) =
        Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults)

        base.OnRequestPermissionsResult(requestCode, permissions, grantResults)
