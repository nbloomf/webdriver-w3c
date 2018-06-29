Getting Started with `webdriver-w3c`
------------------------------------

Hello, and welcome to the wonderful world of browser automation with
WebDriver and Haskell! This module is a brief tutorial on how we can use
use the `webdriver-w3c` library to write Haskell programs that interact
with web pages just like a person would. If you need to test a web
application, or want to automate some web thing that curl and wget alone
can't handle easily, you might find this mildly interesting, maybe.

(This text is a literate program, so we have to start with some compiler
noises. Nothing to see here!)

``` {.sourceCode .literate .haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import Test.Tasty
import qualified System.Environment as SE
import Control.Monad

main :: IO ()
main = return ()
```

Prerequisites
-------------

To follow along, you're going to need a few things.

1.  [Stack](https://docs.haskellstack.org/en/stable/README/). Stack is a
    build tool for Haskell projects. It compiles our programs, runs
    tests, processes documentation, generates code coverage reports, and
    keeps project dependencies under control.
2.  A copy of this repository
3.  A web browser; this tutorial assumes you're using Firefox.
4.  A WebDriver proxy server for your browser. For Firefox this is
    [geckodriver](https://github.com/mozilla/geckodriver). Don't sweat
    it if you don't know what "WebDriver proxy server" means right now,
    we'll get to that.

Next, start your proxy server. For geckodriver on unix-like OSs, that is
done with the `geckodriver &` command. You should see a line that looks
something like this:

    1521524046173   geckodriver INFO    Listening on 127.0.0.1:4444

Leave that program running. Just leave it alone.

Finally, in another shell window, navigate to the directory holding this
repo and say

    stack ghci webdriver-w3c:webdriver-w3c-intro

Well, don't *say* that, out loud. Type it. :) This might take a while
the first time while stack downloads the compiler and libraries it
needs. When it finishes, this command opens a Haskell interpreter with
`webdriver-w3c` loaded so we can play with it. You'll know everything is
okay if you see a line like

    Ok, one module loaded.

followed by a `λ:` prompt. To be sure, try typing in `return` and then
hit (enter). If you see this scary error message:

    <interactive>:1:1: error:
        • No instance for (Show (a0 -> m0 a0))
            arising from a use of ‘print’
            (maybe you haven't applied a function to enough arguments?)
        • In a stmt of an interactive GHCi command: print it

then everything is working great!

My First Browser Automation
---------------------------

Ok! You've got your WebDriver proxy (geckodriver) running in one
terminal window, and ghci running in another. Let's start with a simple
example to illustrate what we can do, then explain how it works. Read
this code block, even if the syntax is meaningless.

``` {.sourceCode .literate .haskell}
do_a_barrel_roll :: WebDriver IO ()
do_a_barrel_roll = do
  fullscreenWindow
  navigateTo "https://www.google.com"
  performActions [typeString "do a barrel roll"]
  performActions [press EnterKey]
  wait 5000000
  return ()
```

Without running that code -- and maybe without being proficient in
Haskell -- what do you think it does?

Now let's run it. In the interpreter, type

    example1

followed by (enter). You should see a Firefox window open, go
fullscreen, and search Google for "do a barrel roll".

`example1`, by the way, is this:

``` {.sourceCode .literate .haskell}
example1 :: IO ()
example1 = do
  execWebDriver defaultWebDriverConfig
    (runIsolated defaultFirefoxCapabilities do_a_barrel_roll)
  return ()
```

Let's break down what just happened.

1.  `do_a_barrel_roll` is a *WebDriver session*, expressed in the
    `WebDriver` DSL. It's a high-level description for a sequence of
    browser actions: in this case, "make the window full screen",
    "navigate to google.com", and so on.
2.  `runIsolated` takes a WebDriver session and runs it in a fresh
    browser instance. The parameters of this instance are specified in
    `defaultFirefoxCapabilities`.
3.  `execWebDriver` takes a WebDriver session and carries out the steps,
    using some options specified in `defaultWebDriverConfig`.

You probably also noticed a bunch of noise got printed to your terminal
starting with something like this:

    λ: example1
    2018-06-23 15:19:46 Request POST http://localhost:4444/session
    {
        "capabilities": {
            "alwaysMatch": {
                "browserName": "firefox"
            }
        },
        "desiredCapabilities": {
            "browserName": "firefox"
        }
    }
    2018-06-23 15:19:48 Response
    {
        "value": {
            "sessionId": "383edca7-3054-0544-8c1e-cc64099462de",
            "capabilities": {
                "moz:webdriverClick": true,
                "platformVersion": "17.4.0",
                "moz:headless": false,
                "moz:useNonSpecCompliantPointerOrigin": false,
                "browserVersion": "60.0.2",
                "rotatable": false,
                "pageLoadStrategy": "normal",
                "moz:profile": "/var/folders/td/sxyy9wl919740vddr49g8nth0000gn/T/rust_mozprofile.aleh5JscOwwI",
                "moz:accessibilityChecks": false,
                "moz:processID": 88470,
                "platformName": "darwin",
                "timeouts": {
                    "implicit": 0,
                    "script": 30000,
                    "pageLoad": 300000
                },
                "acceptInsecureCerts": false,
                "browserName": "firefox"
            }
        }
    }

This is the log. WebDriver sessions keep track of a bunch of info to
help with debugging, like all requests and responses and all raised
errors. By default the logs are printed to stderr but this is
configurable.

So what can you do in a WebDriver session? Not much -- but this is by
design. The library includes:

-   A binding for each endpoint in the WebDriver spec
-   Some basic functions for reading and writing files, reading and
    writing at the console, and making arbitrary HTTP requests

This plus Haskell's `do` notation make for a tidy EDSL for running
browsers. Notably, a `WebDriver` session cannot do arbitrary `IO` by
default, and `WebDriver` sessions are pure values. (There is an escape
hatch for this restriction.)

Behind the Scenes
-----------------

WebDriver is an HTTP API for controlling web browsers like a human user
would. In principle a browser could implement this API directly. In
practice the major browsers have their own internally maintained APIs
for automation and use a *proxy server* to translate between WebDriver
and their internal API.

This is the role geckodriver is playing in our examples so far: deep
down, our code is making HTTP requests to geckodriver, and geckodriver
is passing these requests on to Firefox.

This library is also tested against Chrome via chromedriver. To do that,
using `chromedriver`'s default settings, we need to make a couple of
adjustments to the examples: replace

    defaultWebDriverConfig

by

    defaultWebDriverConfig
      { _env = defaultWDEnv
        { _remotePort = 9515
        , _responseFormat = ChromeFormat
        }
      }

and replace

    defaultFirefoxCapabilities

by

    defaultChromeCapabilities

(By the way - `defaultWebDriverConfig` has type `WebDriverConfig`, and
includes knobs for tweaking almost everything about how our sessions
run.)

Making Assertions
-----------------

It's expected that you're probably interested in using browser
automation to run end-to-end tests on some web application -- and
`webdriver-w3c` has some extra bits built in to make this simpler.

In addition to the usual browser action commands, you can sprinkle your
`WebDriver` sessions with *assertions*. Here's an example.

``` {.sourceCode .literate .haskell}
what_page_is_this :: (Monad eff) => WebDriver eff ()
what_page_is_this = do
  navigateTo "https://www.google.com"
  title <- getTitle
  assertEqual title "Welcome to Lycos!" "Making sure we're at the lycos homepage"
  return ()
```

Note the signature: `(Monad eff) => WebDriver eff ()` instead of
`WebDriver IO ()`. What's happening here is that `WebDriver` is
parameterized by the monad that effects (like writing to files and
making HTTP requests) take place in. These effects are "run" by an
explicit evaluator that, for the default configuration, happens to use
`IO`, but both the effect monad and the evaluator are configurable. By
swapping out `IO` for another type we can, for example, run our tests
against a mock Internet, and swapping out the evaluator we might have a
"dry run" evaluator that doesn't actually do anything, but logs what it
would have done. It's good practice to make our `WebDriver` code
maximally flexible by using an effect parameter like `eff` instead of
the concrete `IO` unless there's a good reason not to.

Anyway, back to the example. What do you think this code does? Let's try
it: type

    example2

in the interpreter. You should see a browser window open briefly to
google.com, with a scary "Invalid Assertion" message in the interpreter.
`assertEqual` is the assertion statement: it takes two things (strings
in this case) and checks whether they are equal. Shocking, hm? The third
argument to `assertEqual` is a *comment*, so we can include some human
readable info as to *why* this assertion was made.

This is `example2`:

``` {.sourceCode .literate .haskell}
example2 :: IO ()
example2 = do
  (_, result) <- debugWebDriver defaultWebDriverConfig
    (runIsolated defaultFirefoxCapabilities what_page_is_this)
  printSummary result
  return ()
```

Here's what happened:

1.  `what_page_is_this` is a WebDriver session, just like
    `do_a_barrel_roll`, this time including an assertion: that the title
    of some web page is "Welcome to Lycos!".
2.  `runIsolated` runs `what_page_is_this` in a fresh browser instance.
3.  `debugWebDriver` works much like `execWebDriver`, except that it
    collects the results of any assertion statements and summarizes them
    (this is `result`).
4.  `printSummary` takes the assertion results and prints them out all
    pretty like.

Documentation on assertions is on
[Hackage](https://hackage.haskell.org/package/webdriver-w3c-0.0.1/docs/Web-Api-WebDriver-Assert.html).

Suites of Tests
---------------

Alright. If you're writing e2e tests, you probably want to write a *lot*
of e2e tests. In this case, we'd like our tests to be modular, isolated,
and well-organized, so that when things go wrong we can quickly diagnose
what happened. For this, `webdriver-w3c` integrates with the
[tasty](https://hackage.haskell.org/package/tasty) test framework --
just import `Test.Tasty.WebDriver`.

Suppose we've got two WebDriver tests. These are pretty dweeby just for
illustration's sake.

``` {.sourceCode .literate .haskell}
back_button :: (Monad eff) => WebDriver eff ()
back_button = do
  navigateTo "https://www.google.com"
  navigateTo "https://wordpress.com"
  goBack
  title <- getTitle
  assertEqual title "Google" "Behavior of 'back' button from WordPress homepage"
  return ()

refresh_page :: (Monad eff) => WebDriver eff ()
refresh_page = do
  navigateTo "https://www.mozilla.org"
  pageRefresh
  title <- getTitle
  assertEqual title "Mozilla's Epic HomePage on the Internets"
    "Refresh mozilla.org"
  return ()
```

We can organize them into a hierarchy of tests like so.

``` {.sourceCode .literate .haskell}
test_suite :: TestTree
test_suite = testGroup "All Tests"
  [ testCase "Back Button" back_button
  , testCase "Refresh" refresh_page
  ]
```

Try running the suite with

    example3

in the interpreter. Here's what `example3` looks like:

``` {.sourceCode .literate .haskell}
example3 :: IO ()
example3 = do
  SE.setEnv "TASTY_NUM_THREADS" "1"
  defaultWebDriverMain
    $ localOption (SilentLog)
    $ localOption (PrivateMode True)
    $ test_suite
```

Here's what happened:

1.  `test_suite` is a Tasty tree of individual `WebDriver` test cases.
2.  `defaultWebDriverMain` is a Tasty function that runs test trees. In
    this case we've also used `localOption` to tweak how the tests run
    -- suppressing the usual session log output.

Tasty gave us lots of nice things for free, like pretty printing test
results and timings.

    λ: example3
    >>> Deployment environment is DEV
    >>> Logging with colors
    All Tests
      Back Button: OK (7.23s)
        1 assertion(s)
      Refresh:     FAIL (4.29s)
        Invalid Assertion 
        assertion: "Internet for people, not profit \8212 Mozilla" is equal to "Mozilla's Epic HomePage on the Internets" 
        comment: Refresh mozilla.org

    1 out of 2 tests failed (11.53s)

Other test case constructors and test options are available; see
[Hackage](https://hackage.haskell.org/package/webdriver-w3c-0.0.1/docs/Test-Tasty-WebDriver.html)
for the details.

The test suite for `webdriver-w3c` itself uses the Tasty integration.
There is also a function, `checkWebDriver`, that can be used to build
tests with QuickCheck, if you don't find that idea abominable. :)

We need more power!
-------------------

The vanilla `WebDriver` is designed to help you control a browser with
*batteries included*, but it has limitations. It can't possibly
anticipate all the different ways you might want to control your tests,
and it can't do arbitrary `IO`. But we have a powerful and very general
escape hatch: `WebDriver` is a special case of the `WebDriverT` monad
transformer.

The actual definition of `WebDriver` is

    type WebDriver eff a = WebDriverT (IdentityT eff) a

where `IdentityT` is the *inner monad* in transformer terms -- actually
it's an inner monad transformer, on the effect monad `eff`. By swapping
out `IdentityT` for another transformer we can add features specific to
our application.

Here's a typical example. Say you're testing a site with two deployment
tiers -- "test" and "production". For the most part the same test suite
should run against both tiers, but there are minor differences. Say the
base URLs are slightly different; maybe production lives at
`example.com` while test lives at `test.example.com`. Also while
developing a new feature some parts of the test suite should only run on
the test tier, maybe controlled by a feature flag.

What we need is some extra read-only state to pass around. We can do
this with a `ReaderT` transformer. To avoid adding a dependency on a
whole transformer library, lets roll our own:

``` {.sourceCode .literate .haskell}
data ReaderT r eff a = ReaderT
  { runReaderT :: r -> eff a
  }

instance (Monad eff) => Monad (ReaderT r eff) where
  return x = ReaderT $ \_ -> return x

  x >>= f = ReaderT $ \r -> do
    a <- runReaderT x r
    runReaderT (f a) r

instance (Monad eff) => Applicative (ReaderT r eff) where
  pure = return
  (<*>) = ap

instance (Monad eff) => Functor (ReaderT r eff) where
  fmap f x = x >>= (return . f)

liftReaderT :: (Monad eff) => eff a -> ReaderT r eff a
liftReaderT x = ReaderT $ \_ -> x

reader :: (Monad eff) => (r -> a) -> ReaderT r eff a
reader f = ReaderT $ \r -> return $ f r
```

Now our actual state might look something like this:

``` {.sourceCode .literate .haskell}
data MyEnv = MyEnv
  { tier :: Tier
  , featureFlag :: Bool
  }

data Tier = Test | Production

env :: Tier -> MyEnv
env t = MyEnv
  { tier = t
  , featureFlag = False
  }
```

And we can augment `WebDriverT` with our reader transformer.

``` {.sourceCode .literate .haskell}
type MyWebDriver eff a = WebDriverT (ReaderT MyEnv eff) a
```

Now we can build values in `MyWebDriver` using the same API as before,
using the extra features of the inner monad with `liftWebDriverT`.

``` {.sourceCode .literate .haskell}
custom_environment :: (Monad eff) => MyWebDriver eff ()
custom_environment = do
  theTier <- liftWebDriverT $ reader tier
  case theTier of
    Test -> navigateTo "http://google.com"
    Production -> navigateTo "http://yahoo.com"
```

To actually run sessions using our custom monad stack we need to make a
few adjustments. First, we use `execWebDriverT` instead of
`execWebDriver`. This function takes one extra argument corresponding to
`lift` for the inner transformer.

Second, we need to supply a function that "runs" the inner transformer
(in this case `ReaderT eff a`) to `IO`.

``` {.sourceCode .literate .haskell}
execReaderT :: r -> ReaderT r IO a -> IO a
execReaderT r x = runReaderT x r
```

Running our custom WebDriver monad is then straightforward.

``` {.sourceCode .literate .haskell}
example4 :: Tier -> IO ()
example4 t = do
  execReaderT (env t) $
    execWebDriverT defaultWebDriverConfig liftReaderT
      (runIsolated defaultFirefoxCapabilities custom_environment)
  return ()
```

Try it out with

    example4 Test
    example4 Production

We can similarly use a custom inner monad to check assertions and with
the tasty integration; there are analogous `debugWebDriverT` and
`testCaseT` functions.

`ReaderT` is just one option for the inner monad transformer. We could
put mutable state, delimited continuations, or even another HTTP API
monad in there. Use your imagination!

Where to Learn More
-------------------

For now the canonical documentation is the haddock annotations on
[Hackage](https://hackage.haskell.org/package/webdriver-w3c).
