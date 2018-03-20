Getting Started with `webdriver-w3c`
------------------------------------

Hello, and welcome to the wonderful world of browser automation with WebDriver and Haskell! This module is a brief tutorial on how we can use use the `webdriver-w3c` library to write Haskell programs that interact with web pages just like a person would. If you need to test a web application, or want to automate some web thing that curl and wget alone can't handle easily, you might find this mildly interesting, maybe.

(This text is a literate program, so we have to start with some compiler noises. Nothing to see here!)

> module Main where
> 
> import Web.Api.Http
> import Web.Api.WebDriver
> 
> main :: IO ()
> main = return ()



Prerequisites
-------------

To follow along, you're going to need a few things.

1. [Stack](https://docs.haskellstack.org/en/stable/README/). Stack is a build tool for Haskell projects. It compiles our programs, runs tests, processes documentation, generates code coverage reports, and keeps project dependencies under control. If you've never worked with Haskell before, stack is all you really need to get started -- it can download anything else it needs (compilers, libraries, etc.) on the fly. If you've used Haskell but not stack, you're in for a treat. :)
2. A copy of this repository
3. A web browser; this tutorial assumes you're using Firefox.
4. A WebDriver proxy server for your browser. For Firefox this is [geckodriver](https://github.com/mozilla/geckodriver). Don't sweat it if you don't know what "WebDriver proxy server" means right now, we'll get to that.

Next, start your proxy server. For geckodriver on unix-like OSs, that is done with the `geckodriver` command. You should see a line that looks something like this:

    1521524046173	geckodriver	INFO	Listening on 127.0.0.1:4444

Leave that program running. Just leave it alone.

Finally, in another shell window, navigate to the directory holding this repo and say `stack ghci`. Well, don't *say* that, out loud. Type it. :) This might take a while the first time while stack downloads the compiler and libraries it needs. When it finishes, this command opens a Haskell interpreter with `webdriver-w3c` loaded so we can play with it. You'll know everything is okay if you see a line like

    Ok, 16 modules loaded.

followed by a `λ:` prompt. To be sure, try typing in `return` and then hit (enter). If you see this scary error message:

    <interactive>:1:1: error:
        • No instance for (Show (a0 -> m0 a0))
            arising from a use of ‘print’
            (maybe you haven't applied a function to enough arguments?)
        • In a stmt of an interactive GHCi command: print it

then everything is working great!



My First Browser Automation
---------------------------

Ok! You've got your WebDriver proxy (geckodriver) running in one terminal window, and ghci running in another. Let's start with a simple example to illustrate what we can do, then explain how it works. Read this code block, even if the syntax is meaningless.

> do_a_barrel_roll :: WebDriver IO ()
> do_a_barrel_roll = do
>   fullscreenWindow
>   navigateTo "https://www.google.com"
>   performActions [typeString "do a barrel roll"]
>   performActions [press EnterKey]
>   mPauseInSeconds 7
>   return ()

Without running that code -- and maybe without being proficient in Haskell -- what do you think it does?

Now let's run it. In the interpreter, type

    example1

followed by (enter). You should see a Firefox window open, go fullscreen, and search Google for "do a barrel roll".

`example1`, by the way, is this:

> example1 :: IO ()
> example1 = do
>   runSession defaultWebDriverConfig
>     (runIsolated defaultFirefoxCapabilities do_a_barrel_roll)
>   return ()

Let's break down what just happened.

1. `do_a_barrel_roll` is a *WebDriver session*, expressed in the `WebDriver IO` DSL. It's a high-level description for a sequence of browser actions: in this case, "make the window full screen", "navigate to google.com", and so on.
2. `runIsolated` takes a WebDriver session and runs it in a fresh browser instance. The parameters of this instance are specified in `defaultFirefoxCapabilities`.
3. `runSession` takes a WebDriver session and carries out the steps, using some options specified in `defaultWebDriverConfig`.

You probably also noticed a bunch of noise got printed to your terminal: this is the log. WebDriver sessions keep track of all requests and responses, as well as a bunch of other information, to help with debugging. By default the logs are printed to stderr but this is configurable.

So what can you do in a WebDriver session? Well, anything you want with `liftIO`. But this library provides some built in commands:

* A binding for each endpoint in the WebDriver spec (almost: the last handful are coming soon)
* Some basic functions for reading and writing files, reading and writing at the console, getting random data, and making arbitrary HTTP requests

Right now the best place to learn about these is the generated Haddock documentation.



Making Assertions
-----------------

It's expected that you're probably interested in using browser automation to run end-to-end tests on some web application -- and webdriver-w3c has some extra bits built in to make this simpler.

In addition to the usual browser action commands, you can sprinkle your `WebDriver` sessions with *assertions*. Here's an example.

> what_page_is_this :: WebDriver IO ()
> what_page_is_this = do
>   navigateTo "https://www.google.com"
>   title <- getTitle
>   assertEqual title "Welcome to Lycos!" "Making sure we're at the lycos homepage"
>   return ()

What do you think this code does? Let's try it: type

    example2

in the interpreter. You should see a browser window open briefly to google.com, with a scary "Invalid Assertion" message in the interpreter. `assertEqual` is the assertion statement: it takes two things (strings in this case) and checks whether they are equal. Shocking, hm? The third argument to `assertEqual` is a *comment*, so we can include some human readable info as to *why* this assertion was made.

This is `example2`:

> example2 :: IO ()
> example2 = do
>   result <- debugSession defaultWebDriverConfig
>     (runIsolated defaultFirefoxCapabilities what_page_is_this)
>   printSummary $ summarize result
>   return ()

Here's what happened:

1. `what_page_is_this` is a WebDriver session, just like `do_a_barrel_roll`, this time including an assertion: that the title of some web page is "Welcome to Lycos!".
2. `runIsolated` runs `what_page_is_this` in a fresh browser instance.
3. `debugSession` works just like `runSession`, except that it collects the results of any assertion statements and returns them (this is `result`).
4. `printSummary` takes the assertion results and prints them out all pretty like.

So what kinds of assertions can be made? The best place to learn about these is in the generated Haddock documentation.



Suites of Tests
---------------

Alright. If you're writing e2e tests, you probably want to write a *lot* of e2e tests. In this case, we'd like our tests to be modular, isolated, and well-organized, so that when things go wrong we can quickly diagnose what happened. The library has some extra bits for this as well.

Suppose we've got two WebDriver tests. These are pretty dweeby just for illustration's sake.

> back_button :: WebDriver IO ()
> back_button = do
>   navigateTo "https://www.google.com"
>   navigateTo "https://wordpress.com"
>   goBack
>   title <- getTitle
>   assertEqual title "Google" "Behavior of 'back' button from WordPress homepage"
>   return ()
> 
> refresh_page :: WebDriver IO ()
> refresh_page = do
>   navigateTo "https://www.mozilla.org"
>   pageRefresh
>   title <- getTitle
>   assertEqual title "Mozilla's Epic HomePage on the Internets"
>     "Refresh mozilla.org"
>   return ()

We can organize them into a hierarchy of tests like so.

> test_suite :: TestTree (WebDriver IO ())
> test_suite = TestLabel "All Tests" $ TestGroup
>   [ TestLabel "Back Button" $ TestCase back_button
>   , TestLabel "Refresh" $ TestCase refresh_page
>   ]

`TestTree` is just a labeled rose tree -- it knows nothing about WebDriver, assertions, or any of that.

Try running the suite with

    example3

in the interpreter. Here's what `example3` looks like:

> example3 :: IO ()
> example3 = do
>   summary <- debugSuites
>     defaultWebDriverConfig
>     defaultFirefoxCapabilities
>     (return ())
>     (return ())
>     [test_suite]
>   printSummary summary

Here's what happened:

1. `test_suite` is a tree of individual `WebDriver` sessions. Each node in the tree is either an individual test (`TestCase`), a list of test trees (`TestGroup`), or a named test tree (`TestLabel`).
2. `debugSuites` runs a list of test trees -- here there's only one. Note the reappearance of `defaultWebDriverConfig` and `defaultFirefoxCapabilities`; these are used for running the individual tests, just like in the previous examples. Those two `return ()` statements are placeholders for additional setup and teardown code, like auth or deleting saved screenshots, to run before and after the test trees.
3. When run, `debugSuites` now uses the `TestLabel` info from our test trees to provide nested context on any failed assertions.



Using `WebDriver` Interactively
-------------------------------

One more feature of the library is handy when we're writing tests: you can run a WebDriver session interactively from inside ghci. Two additional commands are needed for this: `wdshInit` and `httpShell`.

Start by typing the following two commands in the interpreter.

    λ: ref <- wdshInit defaultWebDriverConfig
    λ: let sh = httpShell ref

Now `sh` is a function that runs WebDriver sessions inside an implicit context stored in `ref`, and overwrites `ref` with updated state after the session runs. The bottom line is that we can run WebDriver commands one at a time in the interpreter -- just prefix them with `sh $`. Try this sequence of commands.

    λ: -- open a new firefox instance
    λ: sh $ openSession defaultFirefoxCapabilities
    λ: -- open google.com
    λ: sh $ navigateTo "https://google.com"
    λ: sh $ performActions [typeString "do a barrel roll"]
    λ: sh $ closeSession

This is handy for building new tests, as we can inspect the browser state (say with developer tools) after each step.



Where to Learn More
-------------------

The canonical source is the generated haddock documentation. Until this is uploaded to hackage, the only way to get the docs is to run

    stack haddock

in this directory and point your browser to the local URL that command emits under "Updating Haddock index for local packages and dependencies in..."
