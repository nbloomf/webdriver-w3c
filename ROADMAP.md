* Chromedriver compatibility: several tests are ignored for chromedriver due to spec-noncompliance. Some of these can be fixed -- notably the findElement tests -- by figuring out how to interpret chromedriver's responses.
* Stealth commands should not log request/response details. We should test that this is the case.
* /session/{session id}/element/{element id}/displayed
* Need a ci test matrix, but have to think about how to prevent combinatorial blowup; dependencies are geckodriver+firefox+chromedriver+chrome, so matrix will get big fast. Compromise: only support one version of the drivers at a time?
* todo.txt => roadmap.md
