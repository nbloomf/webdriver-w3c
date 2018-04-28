About the Tests
===============

Tests for this library fall into two buckets: API tests, where the correct behavior is dictated by the [WebDriver spec](https://www.w3.org/TR/webdriver/); and non-API tests, which cover other behaviors of the library outside the scope of the spec. The API tests are the more important of the two.

The API test suite consists of a list of WebDriver scripts in the [test/Web/Api/WebDriver/Monad/Test/Session](https://github.com/nbloomf/webdriver-w3c/tree/master/test/Web/Api/WebDriver/Monad/Test/Session) directory. These tests are divided into files by expected result; e.g. [Success](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Session/Success.hs) consists of the scripts that should succeed, [UnknownError](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Session/UnknownError.hs) consists of the scripts that should fail with "unknown error", and so on.

API test scripts are run against two drivers: [geckodriver](https://github.com/mozilla/geckodriver) and a mocked remote end implementation. Later versions will also test against Chrome. The code for the mocked remote end is mainly in and under the [test/Web/Api/WebDriver/Monad/Test/Server](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/WebDriver/Monad/Test/Server.hs) namespace, with additional scaffolding under [test/Web/Api/Http/Effects/Test/Mock](https://github.com/nbloomf/webdriver-w3c/blob/master/test/Web/Api/Http/Effects/Test/Mock.hs).
