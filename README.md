# Clath

Clath is single sign-on middleware for Clack. It allows basic login with OAuth1.0a, OAuth2 and OpenID Connect.

Clath currently supports authentication from Google, Twitter, LinkedIn, StackExchange, Reddit and Github.

## Credits

Clath's OAuth2 and OpenID Connect support is based on code written by 
[fiddlerwoaroof](https://github.com/fiddlerwoaroof/cl-oid-connect).
OAuth1.0a support comes from [North](https://github.com/Shinmera/north)

# Usage

Clath runs as middleware in your Clack web application. Add this line to your app *after* the session middleware:

    (clath:component "https://yoursite.somewhere/")

The first parameter is your server URL, which Clath needs to know. The :extension keyword argument, which defaults to "clath/", determines where Clath will mount.

If you are using Caveman2, it is probably best to add this to your `app.lisp` file immediately after the line with `:session`.

Visit "/clath/login/" on your server to see a list of working providers. Initially this will be empty.

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
    > (clath:initialize-secrets)

## Hints

All of these providers require a working user account. Log in to get started.

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
- https://www.reddit.com/prefs/apps/

### Stackexchange

- https://api.stackexchange.com/
- Register For An App Key
- In addition to the Client-ID and Secret, StackExchange requires a key. Save this under :key

    > (setf (ubiquitous:value :stackexchange :key) "[your key]")

### Twitter

- https://apps.twitter.com/
- Create New App
- Twitter uses OAuth1.0a, so the callback URL takes the form `https://yoursite.somewhere/clath/callback1a/twitter`

### X.com

- https://developer.x.com/en/portal/dashboard
- Projects & Apps -> Add Project 

### Facebook

- https://developers.facebook.com/apps/

### Yahoo

- https://developer.yahoo.com/oauth/
- My Apps -> YDN apps -> Create a new app
- https://developer.yahoo.com/apps/
- NOT IMPLEMENTED YET

## Useables

On successful login, Clath will set a few values of interest in the user's session. These are:

### :username

This is a string of the format: unique-id@provider. The provider will supply a generated unique ID for each user, but there's no guarantee that IDs from different providers won't overlap so Clath appends the provider name.

### :provider

This is the keyword representation of the provider currently in use.

### :display-name

Clath attempts to guess what display name the user will prefer based on various fields from the provider. You should probably verify this with the user.

### :clath-userinfo

An assoc-list containing the parsed raw output from the userinfo request. In the world of 3rd party authentication creativity rather than consistency is the preeminent virtue. Extracting meaningful information from the uniquely named fields of each provider is an adventure. Mostly, you should be able to leave this field alone.

## Destination on Login

On successful login, Clath should send the user somewhere. The mechanism to determine this location is hacked together, has some issues, and may change in the future. For now:

- If `*login-destination-hook*` contains a function, the user's identity will be passed to that function as the :username keyword and the session will be passed in :session. The user will be redirected to the URL returned by the function.
- Next, `*login-destination*` will be checked.
- If it is null, :clath-destination in the current session will be checked.
- If all of these fail, Clath will redirect the user to "/"

The login URL will accept a destination parameter. This is placed in the `*login-destination*` for later reference. If the login URL is being generated for the logout page, the destination should probably not be set, lest the user get stuck in a loop. Clath will set `*in-logout-page*` when the logout page function is called. The code that generates the login link should check for it.

Clath will also watch for 403 responses coming from down chain and display the not-logged-in page. When it does so, it stores the current URL in the session under :clath-destination.

## Logging Out

To log out, visit `/clath/logout`.

## Redecorating the Pages

Clath's default login, logout and not-logged-in pages are extremely bare. You can redecorate them by redefining the clath-page-wrapper function. This is the default wrapper:

    (defun clath:clath-page-wrapper (title body-func)
      (with-html-output-to-string (s)
        (:html
          (:head (:title title))
          (:body (str (funcall body-func))))))

Your function is expected to receive a string as the first parameter and a function that takes no parameters as the second parameter. This function will return the inner content of the page as a string when called. Your function should return the complete web page as a string.

Further customization can be done to the individual pages by redefining clath-login-page, clath-logout-page and clath-not-logged-page.

# TODO

- More providers are always nice. 
- There aren't provider buttons yet.

# Author

Ben McGunigle (bnmcgn at gmail.com)

# Copyright

Copyright (c) 2017-2024 Ben McGunigle

# License

Apache License version 2.0

