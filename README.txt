This is the stub README.txt for the "clath" project.

Registering OAuth apps:

-You need to register your website as an app with each login provider. You
will be given an ID and SECRET by the login provider. When these are installed
in the correct place in ubiquitous, authentication will become available from
that provider.

Hints:

Facebook:

https://developers.facebook.com/apps/

Github:
-Log in to your account
-Settings -> OAuth applications (look under Developer settings) ->
 Register a new application
-If http://yoursite.somewhere/ is your site, then:

http://yoursite.somewhere/clath/callback/github

is your callback url.

Google:

-Main Google documentation is here:
https://developers.google.com/identity/protocols/OpenIDConnect
-Actual setup happens here:
https://console.developers.google.com/apis/credentials

-Callback url:

-http://yoursite.somewhere/clath/callback/google

Reddit:

-Log in to your account
-Preferences -> Apps -> are you a developer? create an app...

Yahoo:

-https://developer.yahoo.com/oauth/
-My Apps -> YDN apps -> Create a new app
-Note: callback URL can't have string "yahoo" in it.
-Note: callback URL must exist on public internet, so no local testing.
-NOT IMPLEMENTED YET

Stackexchange:

https://api.stackexchange.com/
-Register For An App Key

Twitter:

https://apps.twitter.com/
- Create New App

