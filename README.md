webdriver-w3c
=============

[![Build Status](https://travis-ci.org/nbloomf/webdriver-w3c.svg?branch=master)](https://travis-ci.org/nbloomf/webdriver-w3c)

Haskell bindings for the W3C WebDriver API


What is it?
-----------

`webdriver-w3c` is a Haskell library providing bindings to the WebDriver API, enabling us to write Haskell programs that control web browsers. It is actively tested against `geckodriver` and `chromedriver`, as well as a fake remote end implementation. It is implemented as a monad transformer.

Also included is an integration with the [tasty](https://hackage.haskell.org/package/tasty) test framework.

[WebDriver](https://www.w3.org/TR/webdriver/) is an HTTP API for interacting with a web browser remotely. It is on track to become a W3C specification and based on work done by the [Selenium](https://www.seleniumhq.org/) community.


Who is it for?
--------------

If you:

* Are interested in browser automation, especially for testing,
* Want to write browser automation code in Haskell, and
* Don't mind filing bug and feature requests,

then you might give `webdriver-w3c` a try.

This library is unrelated to [webdriver](http://hackage.haskell.org/package/webdriver) except in spirit. That library is older and more mature, and depending on your needs may be more appropriate.


Where is the documentation?
---------------------------

Depends on what you want!

* _A cursory glance:_ This brief [tutorial](https://github.com/nbloomf/webdriver-w3c/blob/master/doc/Tutorial.md) shows how to go from nothing to one very simple test.
* _To start a simple project:_ If you want to write a test suite, there's a separate tutorial on using the [tasty integration](https://github.com/nbloomf/webdriver-w3c/blob/master/doc/TastyDemo.md).
* _To dig into the API:_ The API docs will eventually be on Hackage.
* _To mess with the library code:_ There's a very small amount of [developer documentation](https://github.com/nbloomf/webdriver-w3c/blob/master/dev/doc.md); I'm also happy to answer questions.


Who is responsible for this?
----------------------------

Nathan Bloomfield (@nbloomf) wrote and maintains the code.

Other contributors, in lexicographic order:

* Ivan Enderlin (@hywan)

And users like you!

Also thanks to my employer for allowing -- actually, requiring :) -- this to be open source.

And special thanks to Jonathan Lipps' [Simplified Guide](https://github.com/jlipps/simple-wd-spec) to the WebDriver spec.
