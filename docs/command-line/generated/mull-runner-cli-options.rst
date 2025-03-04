--test-program path		Path to a test program

--workers number		How many threads to use

--timeout number		Timeout per test run (milliseconds)

--report-name filename		Filename for the report (only for supported reporters). Defaults to <timestamp>.<extension>

--report-dir directory		Where to store report (defaults to '.')

--reporters reporter		Choose reporters:

    :IDE:	Prints compiler-like warnings into stdout

    :SQLite:	Saves results into an SQLite database

    :Elements:	Generates mutation-testing-elements compatible JSON file

    :Patches:	Generates a unified patchfile for each mutation

--ide-reporter-show-killed		Makes IDEReporter to also report killed mutations (disabled by default)

--debug		Enables Debug Mode: more logs are printed

--strict		Enables Strict Mode: all warning messages are treated as fatal errors

--no-test-output		Does not capture output from test runs

--no-mutant-output		Does not capture output from mutant runs

--no-output		Combines -no-test-output and -no-mutant-output

