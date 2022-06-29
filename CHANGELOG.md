Changelog for webdriver-w3c
===========================

Unreleased
----------

0.0.3
-----

* Added
  * `MonadIO` and `MonadFail` instances for `WebDriverTT t eff`
  * New endpoints: `newWindow`, `getComputedRole`, `getComputedLabel`, `printPage`
  * Compiles with aeson >=2.0.0.0 and GHC >=8.8.1
* Changed
  * The old behavior of `runIsolated` has been renamed to `runIsolated_`, and `runIsolated` now returns the result of its argument. The naming is meant to mimic the `sequence_`/`sequence` pattern.
  * `chromeOptions` renamed to `goog:chromeOptions` in `ToJSON` `FromJSON` instances for `Capability` for compatibility with chromedriver versions >=75; see https://chromedriver.storage.googleapis.com/75.0.3770.8/notes.txt. Fixes issue #21.
* Fixed
  * Bug in behavior of `switchToFrame` when using `FrameContainingElement`
  * Default value of wd-private-mode tasty flag changed to `False`



0.0.2
-----

This version introduces significant changes to the API, prompted by changes in the `script-monad` dependency. The main change is that `WebDriver` and `WebDriverT` have been replaced by `WebDriverT` and `WebDriverTT` and are a more sensible monad transformer and monad transformer transformer, respectively. The main effect of this is that (1) `WebDriver*` types take an extra parameter for the effect monad, and (2) functions for working with `WebDriver*` now have additional `Monad` and `MonadTrans` constraints. The library will now only compile with GHC >=8.6 due to a transitive dependency on `QuantifiedConstraints`.

* Added
  * Browser preferences field on `FirefoxOptions` and `ChromeOptions`
  * `readDataFile`, `writeDataFile`, `readJsonFile`, and `writeJsonFile` data helpers
  * `breakpoint` and `breakpointWith` for helping with debugging; controlled by `breakpointsOn`, and `breakpointsOff`
  * `expectIs`
* Changed
  * Switched order of arguments for `elementSendKeys`, `getElementAttribute`, `getElementProperty`, and `getElementCssValue`. The element reference now comes last to make it easier to chain these with `>>=`.
  * `logDebug` and `logNotice`
  * Tested on geckodriver 0.23.0.
* Fixed
  * Bug in behavior of `cleanupOnError` was causing it to miss some errors, which left the remote end session open



0.0.1
-----

* Added
    * `WebDriver` monad for remotely controlling user agents. Also comes in monad transformer flavor with `WebDriverT` 
    * Bindings for all [WebDriver endpoints](https://w3c.github.io/webdriver/webdriver-spec.html) as of 2018-04-20
    * Integration with the [Tasty](https://hackage.haskell.org/package/tasty) test framework
