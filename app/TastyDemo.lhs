Using `webdriver-w3c` with `tasty`
==================================

It's possible to run "raw" WebDriver sessions, but it's much more convenient to use the [tasty](https://hackage.haskell.org/package/tasty) integration. With tasty we can incorporate WebDriver tests alongside, say, quickcheck and HUnit tests, and get test reports, stats, filtering, and parallel execution for free.

This module demonstrates how to set up a basic test executable and configure it with command line options.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where

We'll need some imports. These are the usual `tasty` modules:

> import Test.Tasty
> import Test.Tasty.ExpectedFailure

These are the `webdriver-w3c` modules:

> import Web.Api.WebDriver

And this is the module that integrates the two.

> import Test.Tasty.WebDriver


Define your tests
-----------------

First things first: to make a WebDriver test suite, we need some WebDriver tests. These are just values of type `WebDriverT IO ()`. (Or more generally, `(Monad eff, Monad (t eff), MonadTrans t) => WebDriverTT t eff ()`, but that's not important for now.) Here are a few dweeby examples. It's not necessary for the tests to start with `_test` or use snake_case; I'm doing it here out of habit.

> _test_one :: (Monad eff) => WebDriverT eff ()
> _test_one = do
>   navigateTo "https://google.com"
> 
> _test_two :: (Monad eff) => WebDriverT eff ()
> _test_two = do
>   navigateTo "https://yahoo.com"
>   assertSuccess "time travel achieved"


Define your `main`
------------------

As usual, our program starts with `main`.

The simplest way to make a test executable with tasty is to use `defaultWebDriverMain`, which has the following signature:

    defaultWebDriverMain :: TestTree -> IO ()

This function wraps tasty's `defaultMain`, which handles command line option parsing, and adds some magic of its own. `TestTree` is tasty's type for an executable test suite. There are several functions for building these out of WebDriver sessions; they live in `Test.Tasty.WebDriver` and have names starting with `testCase`.

Here's an example `main`.

> main :: IO ()
> main = defaultWebDriverMain $
>   testGroup "Test Demo"
>     [ testCase "navigate to google.com" _test_one
>     , testCase "check status" _test_two
>     , ifDriverIs Chromedriver ignoreTest $
>         testCase "navigate to google.com" _test_one
>     ]

We can run different sets of tests based on the value of an option using `askOption`, and we can change the value of an option locally using `localOption`. Changing one option based on the value of another option is a common task; for example, some tests should run differently in headless mode.

Several common uses of the `askOption` pattern are defined in `Test.Tasty.WebDriver`; for instance, the helper function `ifDriverIs` lets us adjust options for different drivers, and `ifTierIs` lets us change behavior between development and testing deployments.


Start your remotes
------------------

To actually run our test suite, we need to have at least one _remote end_ running. These are the proxy servers that accept WebDriver requests via http and reach into a browser to make it do stuff. For now, the library has only been tested with geckodriver and chromedriver. Make sure you've got one or both of those installed, then kick off an instance of each one.

For example,
```
geckodriver > /dev/null 2> /dev/null &
```
starts a geckodriver instance in the background (but suppresses its otherwise voluminous debug output).

You'll want to take note of which host and port your remote end is listening on. By default, geckodriver listens on localhost, port 4444, and chromedriver listens on 9515.


Run your tests
--------------

This demo executable is named `wd-tasty-demo`. If you install and run it, you'll get an error message:

    Error: no remotes defined for geckodriver

What does this mean? To run a webdriver session, we have to tell our program the URIs of the remote ends it should use -- it does not assume one. There are two ways to do this, and you can use either one (or both).

`--wd-remote-ends` lets us supply the remote end URIs on the command line directly. Suppose I've got geckodriver listening on port 4444 and chromedriver on port 9515 (which they do by default). Then I'd use the following option:

    --wd-remote-ends 'geckodriver https://localhost:4444 chromedriver https://localhost:9515'

(Note the explicit `https` scheme; this is required.) This is fine if you have a small number of remote ends running, but the command line quickly gets unwieldy if you have tens or hundreds of remote ends ready to run tests in parallel. So we can also specify the remote end URIs in a specially formatted config file. The config file must look something like this:

    geckodriver
    - https://localhost:4444
    - https://localhost:4445
    chromedriver
    - https://localhost:9515
    - https://localhost:9516

The drivers can come in any order and don't have to be contiguous, and blank lines are ignored. Suppose this file is called `~/.wd/config`; then we supply this to our test executable with the following option:

    --wd-remote-ends-config ~/.wd/config

`webdriver-w3c` can also run your tests in parallel. To take advantage of this, you'll need to compile your executable with `-threaded -rtsopts -with-rtsopts=-N` and start it with the `--num-threads N` option. You'll also need to start more than one remote end of each type. Note that if you want to run N tests in parallel, then you'll need N instances of _each_ remote end (geckodriver and chromedriver) running in the background. This is because the tests are _processed_ sequentially, even if they run in parallel. For instance, if you have 100 firefox tests followed by 100 chrome tests, but run them with one geckodriver and one chromedriver, the tests will run sequentially.

There are a bunch of other command line options for tweaking the behavior of your webdriver tests; use `wd-tasty-demo --help` to see a list. Most of these are pretty specialized. Other options are pretty common. In addition to `--wd-remote-ends` and `--wd-remote-ends-config`, there's `--wd-driver`, for specifying which driver to use, and `--wd-response-format`, which was required when using old versions of chromedriver because it was not fully spec compliant.


Example sessions
----------------

Here are some example commands for running this demo.

Run one at a time with geckodriver:

```
geckodriver --port 4444 > /dev/null 2> /dev/null &
wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444'
```

Run one at a time with geckodriver, but can it with all the logs:

```
geckodriver --port 4444 > /dev/null 2> /dev/null &
wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444' --wd-verbosity silent
```

Run one at a time with chromedriver:

```
chromedriver --port=9515 &
wd-tasty-demo --wd-driver chromedriver --wd-remote-ends 'chromedriver https://localhost:9515'
```

Run two at a time with geckodriver:

```
geckodriver --port 4444 > /dev/null 2> /dev/null &
wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444' --num-threads 2
```
