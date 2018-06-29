# Changelog for webdriver-w3c

## Unreleased changes

* New
  * Browser preferences field on `FirefoxOptions` and `ChromeOptions`
  * `readDataFile`, `writeDataFile`, `readJsonFile`, and `writeJsonFile` data helpers
* Changed
  * Switched order of arguments for `elementSendKeys`, `getElementAttribute`, `getElementProperty`, and `getElementCssValue`. The element reference now comes last to make it easier to chain these with `>>=`.
* Fix
  * Bug in behavior of `cleanupOnError` was not catching all errors


## 0.0.1

* New
    * `WebDriver` monad for remotely controlling user agents. Also comes in monad transformer flavor with `WebDriverT` 
    * Bindings for all [WebDriver endpoints](https://w3c.github.io/webdriver/webdriver-spec.html) as of 2018-04-20
    * Integration with the [Tasty](https://hackage.haskell.org/package/tasty) test framework
