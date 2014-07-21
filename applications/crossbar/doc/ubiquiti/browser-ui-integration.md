# Browser UI Integration
This document describes how to get your webapp hooked up to SSO (single-sign-on)
with some [technical](#technical-bits) and [UX level considerations](#productux-considerations). Oh! and a [list of people to talk to](#contacts).

## Product/UX considerations
**Why use sso at all?**

SSO exists to allow our users not to have to recreate an account for each app at ubnt. It also allows us to consolidate the security considerations to one API.

**Why shouldn't I create my own UI to log the user in directly at my app?**

By providing the user with one UI for logging in it cues them in to which account they should use to log in. They don't know that your account login form is the same one that they are using for all their other apps. 

Additionally it gives the user some level of confidence that when they see our UI that they are at the right place and not somehow being tricked into giving away their information.

**How do I limit access to only some SSO users?**

SSO is an authentication service not an authorization service. Think of a bouncer at a club. Authentication is the bouncer checking your ID. Authorization is checking that you are on the list to get in.

**Yeah that's great, I know what Authentication is. Where do I go to get Authorization?**

Well you can make it up yourself *or* you can use the "groups" service. 

## Technical bits

**How do I log the user in?**

Provide a link to sign-in that points to `https://account.ubnt.com?redirect=<DESTINATION>`. `DESTINATION` should be where you want to send the user after they've logged in. 

**How do I tell that a user is logged in and switch the log-in link for a profile link?**

Make a call to `https://sso.ubnt.com/api/v1/user/self` - if you get a 401 response the user has not logged in. If you get a 200, the body will contain the users basic info. In order for this to work you need to be on a ubnt.com domain. The authentication session cookie is stored on *.ubnt.com.

**I'm getting this CORS error when I try to access sso. What's that all about?**

You need to be on the SSO whitelist to enable Cross Origin Resource Sharing. Contact the devops for sso or any of the other contacts to accomplish this. See the [contact list](#contacts)

**How do I use sso on a non-ubnt.com domain (eg. uwn.com)**

First, your domain needs to be on the cookie-toss whitelist. Contact one of the people on the contact sheet below. Second, when you log your user in pass `token=true` as additional parameter to redirect. When the user is redirected back to your app it will have their auth token attached as an argument. You should set that as a cookie of the same name and parameters as for UBNT.com eg. UBIC_AUTH .ubnt.com http and secure.

**How do I log the user out?**

Very similar to logging in: Provide a link to sign-out that points to `https://account.ubnt.com/logout?redirect=<DESTINATION>`. `DESTINATION` should be where you want to send the user after they've logged out.

**How do I get the User's profile photo? I don't see it in the json.**

Currently we use Lithium to manage our user forums and it manages the user avatar. `http://community.ubnt.com/restapi/vc/users/login/<USERNAME>/profiles/avatar/size/<SIZE>/url?restapi.response_format=json&restapi.response_style=-types` returns a link to the user avatar.
- `USERNAME` - see the json response from sso api `user/self`
- `SIZE` - Choose one: `message`, `profile`, `inline`, `favicon`, `print`

**How do I point to staging sso for my staging environment?**

You'll want to setup a config file with the hostname of the sso server. Deploys should be able to fill in the correct host for the environment and have the entirety of the code follow. for local development `sso-stage.ubnt.com` can be used. localhost is whitelisted for CORS access to stage only. 



## Contacts

 ROLE | NAME | GH
 -----|------|-----
 Product| matt hardy|
 UI Engineer | Patrick Weygand | @derduher
 Devops| Chris Maxwell | @wrathofchris
 API Engineer | Siraaj Khandkar| @ibnfirnas
 
