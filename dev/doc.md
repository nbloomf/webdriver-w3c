Info for Developers
===================

Here's some notes about the implementation of webdriver-w3c.


Setting up an Environment
-------------------------

To have a complete dev environment and run all the tests, you'll need to install [geckodriver](https://github.com/mozilla/geckodriver/releases) and [chromedriver](http://chromedriver.chromium.org/).

For building `webdriver-w3c` itself or your own projects, I recommend [stack](https://docs.haskellstack.org/en/stable/README/) for ease of use. With stack and this repo on your machine, 

* `stack ghci` loads the library in a ghci session
* `make test` runs the tests and generates a code coverage report


Structure of the Library
------------------------

The heart of the library is the `WebDriver` monad, which handles network requests, logging, errors, and other state. The bulk of the API consists of functions into this monad -- one for each endpoint in the WebDriver spec.


About Parallelism
-----------------

If you have a large suite of tests, webdriver-w3c can speed up execution by running the tests in parallel. In fact, running the tests in parallel is no more complicated than running them sequentially. To take advantage of this you'll need to do two things:

First: set the `--num-threads` option or the `TASTY_NUM_THREADS` environment variable higher than 1; this is the number of tests to be run simultaneously. (This also requires compiling your binary with `-threaded -rtsopts -with-rtsopts=-N`; see the [tasty documentation](https://github.com/feuerbach/tasty#running-tests-in-parallel) for details).

Second: have more than one remote end running, and supply their URIs to your test executable. (When running on a CI server, `xvfb-run` is handy here.) You can specify these URIs on the command line directly with syntax like this:

    --remote-ends 'geckodriver: URI URI chromedriver: URI URI'

If your list of remote end URIs is large or dynamically generated, you can also supply it in a config file using the following option:

    --remote-ends-config PATH

where `PATH` is a file formatted like so:

    geckodriver
    - URI
    - URI
    chromedriver
    - URI
    - URI

You can specify the remote end URIs with either `--remote-ends` or `--remote-ends-config` or both. The "blocks" of URIs per driver can be in any order and do not have to be contiguous. Note that the URIs *must* include a scheme (the `https://` part).

The remote end URIs are stored in a set of mutable stacks; one for each driver. On each test run, we atomically pop a remote end URI from the stack, use it to run the tests, and then push the remote URI back onto the stack. If no remote ends are available, the test run waits until one becomes available.

Note that while the tests can be _executed_ in parallel, they are still _processed_ sequentially. In particular, if you've got 100 firefox tests followed by 100 chrome tests, and provide exactly one firefox remote end and one chrome remote end, your tests will not run in parallel. The chrome remote will sit idle until all the firefox tests have finished. To get the full benefit of parallelism, I recommend having N firefox remotes and N chrome remotes and running the tests with N threads; this will not ensure that all 2N remotes are always busy, but will ensure that all N threads are being used.


About the Tests
---------------

Tests for this library fall into two buckets: API tests, where the correct behavior is dictated by the [WebDriver spec](https://www.w3.org/TR/webdriver/); and non-API tests, which cover other behaviors of the library outside the scope of the spec. The API tests are the more important of the two.

The API test suite consists of a list of WebDriver scripts in the [test/Web/Api/WebDriver/Monad/Test/Session](https://github.com/nbloomf/webdriver-w3c/tree/master/test/Web/Api/WebDriver/Monad/Test/Session) directory. These tests are divided into files by expected result; e.g. [Success](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Session/Success.hs) consists of the scripts that should succeed, [UnknownError](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Session/UnknownError.hs) consists of the scripts that should fail with "unknown error", and so on.

API test scripts are run against two drivers: [geckodriver](https://github.com/mozilla/geckodriver) and a mocked remote end implementation. Later versions will also test against Chrome. The code for the mocked remote end is mainly in and under the [test/Web/Api/WebDriver/Monad/Test/Server](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Server.hs) namespace, with additional scaffolding under [test/Web/Api/Http/Effects/Test/Mock](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/Http/Effects/Test/Mock.hs).
