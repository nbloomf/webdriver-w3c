* Chromedriver compatibility: several tests are ignored for chromedriver due to spec-noncompliance. Some of these can be fixed -- notably the findElement tests -- by figuring out how to interpret chromedriver's responses.
* Stealth commands should not log request/response details. We should test that this is the case.
* /session/{session id}/element/{element id}/displayed
* Need a ci test matrix, but have to think about how to prevent combinatorial blowup; dependencies are geckodriver+firefox+chromedriver+chrome, so matrix will get big fast. Compromise: only support one version of the drivers at a time?
* todo.txt => roadmap.md
* no_color env variable

-- refactor cleanup tasks
* log lock in tasty is a just; shouldn't be
* make log printing more configurable in http
* default webdriver config?
* mock server as an option
* deal with timeouts
* logprinterlock option is wrong
* if a color option is set
    then use it.
    else if $NO_COLOR is set
      then use it
      else use color
* swap out _vars in environment for a string. what to call it? (environment is taken)
* artifact directory; use adjustoption in the testcase builders, with an extra argument
  for the directory name, to describe a hierarchy for saving test artifacts (stdout, logs,
  screenshots) and also getting mocked input (stdin, reference screenshots, etc).
  con: need to specify both
  directory name and human readable test name. will need a type for directory names;
  use overloadedstrings and throw an error if it has bad chars. don't clobber old 
  artifact directories; put each in a separate dir with e.g. timestamp in the name
  provide an option for building the directory structure without running the tests.
  This has the advantage of giving a standardized structure to test artifacts for review.
  will need an option for the root artifact directory. e.g.

    root_artifact_dir
    +> run_1970_01_01_00_00_00
    |  +> test_artifact_hierarchy
    |
    +> run_1970_01_01_01_00_00
       +> test_artifact_hierarchy

Notes on ignored tests
- getAlertText on headless geckodriver
  https://bugzilla.mozilla.org/show_bug.cgi?id=1460857

