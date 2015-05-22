# yesod-persistent-session

Server-side session backend using persistent.

This package implement traditional server-side sessions.  Users
who don't have a session yet are assigned a random 144-bit
session ID that is the key on a database table kept by
persistent.  All session data is saved on the database.

The session ID stays fixed most of the time.  Anonymous users
receive session IDs unless their session remains empty (as an
optimization).  The session ID can be invalidated in order to
prevent
[session fixation attacks](http://www.acrossecurity.com/papers/session_fixation.pdf),
either automatically (see below) or manually (via
`forceInvalidate`).


## Authentication integration

We have special support for `yesod-auth`:

  * The `_ID` session key used by `yesod-auth` is recognized and
    saved separately on the database.  This allows you to quickly
    identify all sessions of a given user.  For example, you're
    able to implement a "log out everywhere" button.

  * Whenever the `_ID` changes, the backend will also invalidate
    the current session ID and migrate the session data to a new
    ID.  This prevents session fixation attacks while still
    allowing you to maintain session state accross login/logout
    boundaries.

If you wish to use a different authentication mechanism and still
enjoy the advantages above, just use the same `_ID` session key.


## Current limitations

  * All sessions use persistent cookies.

  * We support SQL backends only, such as
    `persistent-postgresql`.  The code has to fix upfront which
    persistent backend is used.


## Background

Yesod has always support client-side sessions via the
[`clientsession`](http://hackage.haskell.org/package/clientsession)
package: the session data is encrypted, signed, encoded and sent
to the client inside a cookie.  When receiving a request, the
cookie is decoded, verified and decrypted.  The server does not
have to maintain any state, so the client-side session backend is
as fast as the cryptographic primitives.

However, there are some disadvantages to client-side sessions:

  * _Replay attacks_.  It's not possible to invalidate a session,
    for example.  When logging out, a new cookie is sent with
    logged out session data.  However, as the server doesn't
    maintain state about sessions, it will still accept the old,
    logged in cookie until it expires.  One could set very small
    expiration times to mitigate this, but this would force users
    to relogin frequently.  This server-side backend allows you
    to maintain long expiration times while still having secure
    logouts.

  * _Cookie size_.  As the cookie contain the whole session data
    plus some overhead, care must be taken not to create too much
    session data.  Yesod already saves the logged in user ID via
    `yesod-auth` and a XSRF token via `yesod-form`.  This
    server-side backend uses a cookie of fixed size (24 bytes).

  * _No remote logout_.  In many instances it is desirable to
    invalidate sessions other than the current one.  For example,
    the user may have changed their password, or the the site
    provides a button to cancel all logged in sessions besides
    the current one.  This server-side backend allows you to
    invalidate sessions other than the current one via
    `forceInvalidate`.

  * _Missing key rotation_.  Ideally, `clientsession`'s keys
    should be rotated periodically.  In practice, support for key
    rotation has never been implemented on `clientsession`.  This
    server-side backend does not need to do key rotations, and
    the session ID CPRNG is automatically reseeded.

If you're concerned about any of the points above, you've come to
the right package!
