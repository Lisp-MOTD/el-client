## Lisp Message of the Day (Emacs Lisp Client)

### Caveat

This code is not yet complete.  In particular, it tries to fetch
data from a URL that is not yet being served.  Look for a big
announcement on [Planet Lisp][pl].

[pl]: http://planet.lisp.org/

### Overview

During a panel discussion at [ILC 2014][ILC], there was some talk about
how better to foster a cohesive Lisp community.  [Nick Levine][NDL]
said, offhandedly, that there should be some Lisp Message of the Day
that comes up when you start your REPL.

[ILC]: http://ilc2014.iro.umontreal.ca/
[NDL]: http://nicklevine.org/

I really like this idea.  I am offering this implementation of that
idea.  If there is some short message you have that would be of
general interest to the Lisp community, email me.  If you'd like to
get the message of the day when you start your Emacs, put this form in
your init file:

    (require 'motd)
    (lisp-message-of-the-day)

### License

This code is available through the [UNLICENSE][UN].

[UN]: http://unlicense.org/
