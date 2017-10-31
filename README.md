# Clath

Clath is single sign-on middleware for Clack. It allows basic login with OAuth1.0a, OAuth2 and OpenID Connect.

Clath currently supports authentication from Google, Twitter, LinkedIn, StackExchange, Reddit and Github.

## Credits

Clath's OAuth2 and OpenID Connect support is based on code written by 
[fiddlerwoaroof](https://github.com/fiddlerwoaroof/cl-oid-connect).
OAuth1.0a support comes from [North](https://github.com/Shinmera/north)

# Usage

Clath runs as middleware in your Clack web application. Add this line to your app:

    (clath:component "https://yoursite.somewhere/")

The first parameter is your server URL, which Clath needs to know. The :extension keyword argument, which defaults to "clath/", determines where Clath will mount.

Visit "/clath/login" on your server to see a list of working providers. Initially this will be empty.

## Registering with Providers

You need to register your website as an app with each login provider. You
will be given an ID and SECRET by the login provider. When these are installed
in the correct place, authentication from the provider will become available.

## Callback URL

Most providers will require that you supply a callback URL when you register your website. Callback URLs take the form:

    https://yoursite.somewhere/clath/callback/provider

For providers that use OAuth1.0a, replace "callback" with "callback1a". So the callback for Twitter will be:

    https://yoursite.somewhere/clath/callback1a/twitter

## Saving your ClientID and Secret:

Terminology may vary between providers, but there will be an application or client ID. There will also be something that corresponds to the "secret" field. Occasionally there will be a third parameter.

    > (ubiquitous:restore 'clath:clath)
    > (setf (ubiquitous:value :google :client-id) "[your client id]")
    > (setf (ubiquitous:value :google :secret) "[your secret]")

## Hints

All of these providers require a working user account. Log in to get started.

### Facebook

- https://developers.facebook.com/apps/
- NOT IMPLEMENTED YET

### Github

- Settings -> OAuth applications (look under Developer settings) -> Register a new application

### Google

- Main Google documentation is here:
https://developers.google.com/identity/protocols/OpenIDConnect
- Actual setup happens here:
https://console.developers.google.com/apis/credentials

### LinkedIn

- https://www.linkedin.com/secure/developer?newapp=

### Reddit

- Log in to your account
- Preferences -> Apps -> are you a developer? create an app...

### Stackexchange

- https://api.stackexchange.com/
- Register For An App Key
- In addition to the Client-ID and Secret, StackExchange requires a key. Save this under :key

    > (setf (ubiquitous:value :stackexchange :key) "[your key]")

### Twitter

- https://apps.twitter.com/
- Create New App
- Twitter uses OAuth1.0a, so the callback URL takes the form `https://yoursite.somewhere/clath/callback1a/twitter`

### Yahoo

- https://developer.yahoo.com/oauth/
- My Apps -> YDN apps -> Create a new app
- https://developer.yahoo.com/apps/
- Note: callback URL can't have string "yahoo" in it.
- Note: callback URL must exist on public internet, so no local testing.
- NOT IMPLEMENTED YET

## Aftermath

On successful login, Clath will set a few values of interest in the user's session. These are:

### :username

This is a string of the format: unique-id@provider. The provider will supply a generated unique ID for each user, but there's no guarantee that IDs from different providers won't overlap so Clath appends the provider name.

### :provider

This is the keyword representation of the provider currently in use.

### :display-name

Clath attempts to guess what display name the user will prefer based on various fields from the provider. You should probably verify this with the user.

### :clath-userinfo

An assoc-list containing the parsed, raw output from the userinfo request. In the world of 3rd party authentication creativity rather than consistency is the preeminent virtue. Extracting meaningful information from the uniquely named fields of each provider is an adventure. Mostly, you should be able to leave this field alone. 


