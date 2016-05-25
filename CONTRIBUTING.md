# Code Development Guidelines

Expectations for 2600Hz engineers as well as folks contributing to the platform.

## Coding Standards

The most important rule: **Be consistent both with your code and the project.**

If, in the project, we have been using a variable name to represent an object, continue that in your code. Attempt to mirror existing conventions first, even if you may not agree with them! We can discuss changing it globally but every effort should be made to have a unified and consistent code base.

The full coding standard that we tend to use, and will likely adapt shortly in our documentation effort, is available [here](https://github.com/inaka/erlang_guidelines).

Some of the highlights:

* Loud errors
  * Don't let errors and exceptions go unlogged. Even when you handle them, write a logline with the stack trace.
  * If it is a log line that requires manual intervention, or action, then it should should be documented in “runbook” style for a system operator.
* Keep functions small
  * Try to write functions with a small number of expressions, and that do only one thing. 12 expressions per function except for integration tests is a good measure.
* Maintain existing style
  * When editing a module written by someone else, stick to the style in which it was written. If a project has an overall style, stick to that when writing new modules as well.
* Use your spacebar
  * Surround operators and commas with spaces.
* No Trailing Whitespace
  * Remove trailing whitespace from your lines
* Avoid deep nesting
  * Avoid nesting case or any conditionals.  If nesting is required do not exceed three levels.  It is better to call a well-named function to encapsulate the decision making of the inner case.
* Do not use if statements.
  * In some circumstances if introduces static boolean logic in your code, reducing code flexibility. In other cases, a case or a function call with pattern matching in its clauses is just more declarative. For newcomers (that have learned to use if in other languages), Erlang's if can be either hard to understand or easily abused.
* Honor DRY.
  * Don't write the same code in many places, use functions and variables for that.
* More, smaller functions over case expressions
  * Use pattern-matching in function clauses rather than case clauses.
* No God modules
  * Don't design your system using god modules (modules that have a huge number of functions and/or deal with very unrelated things - eg core/whistle/src/wh_util.erl)
* Group modules in subdirectories by functionality
  * When having lots of modules, use subdirectories for them, named with a nice descriptive name for what that "package" does.
* Don't write spaghetti code
  * For example, a list comprehension with a case inside, or blocks with begin/end, and nested stuff
* Don't share your records
  * Records should not be shared among multiple modules. If you need to share objects that are represented as records, use opaque exported types and provide adequate accessor functions in your module.
* Always add types for your records:
  ```erlang
    -record(foo, {bar :: binary()}).
    -type foo() :: #foo{}.
    -type foos() :: [foo()].
   ```
   ```erlang
   -type state() :: #state{}.
   ```
* Types in records
  * Always add type definitions to your record fields
* Properly use logging levels
  * debug: Very low-level info, that may cover your screen and don't let you type in it :P
  * info: The system's life, in some detail. Things that happen usually, but not all the time. You should be able to use the console with acceptable interruptions in this level.
  * notice: Meaningful things that are worth noticing, like the startup or termination of supervisors or important gen_servers, etc…
  * warning: Handled errors, the system keeps working as usual, but something out of the ordinary happened
  * error: Something bad and unexpected happen, usually an exception or error (DO log the stack trace here)
  * critical: The system (or a part of it) crashed and somebody should be informed and take action about it
* Simple unit tests
  *Single responsibility applies to tests as well. When writing unit tests, keep them short and don't put more than 1 or 2 asserts per test

Some 2600Hz specifics:

* Tickie all atoms
  * Use `'foo'` not `foo`
* Write accurate specs for all functions
  * Use the most restrictive type(s) you can.
  ```erlang
  -spec foo(any()) -> any(). % is of no use to anyone!
  ```
  * Run [dialyzer](./dialyzer.md) on any modules you make changes to, plus any modules your new code makes calls to.
* When creating lists or binaries, drop the comma and next element to a new line
  * This includes `-export([...])` directives.
  * No:
  ```erlang
  [this, list, is, wrong]
  ```
  * Yes:
  ```erlang
  [this
  ,list
  ,is
  ,right
  ]
  ```
* Exports should be grouped by functionality.
  * Do not export all functions nor add all exports to a single export directive.
* Code should be self documenting.
  * It is not a forum for you to prove how amazing you are at the language.
  * We always prefer readability over complexity. Which means simple language constructs and explicit variable/function names.
    * `AccountId` instead of `Id` or `AcctId`
* The function name should clearly describe what it will do.
  * Common patterns include:
    * The use of plural to singular.  For example, `process_accounts` calls to `process_account` for each account.
    * The use of `maybe_X` executing `X` if a condition is met.  For example, `maybe_process_accounts` calls to `process_accounts` if there are accounts present in the system.
* Never introduce a single letter variable.
  * It is marginally acceptable to use a single letter in “inner” scope variables but they should match the first letter of a fully named variable they are shadowing. For example, in the predicate function it is OK to use `A` to represent `Account` if the `Account` variable is used outside the predicate.
* Do not chain functions
  * Do not call the next function of a sequence of functions from the previous function.
  * If you need to chain functions together to make a sequence fold over a list of functions.
  * No:
  ```erlang
  do_x() ->
      X = lookup(),
      do_y(X).
  do_y(X) ->
      Y = transform_x(X),
      do_z(Y).
  do_z(Y) ->
      apply_z(Y).
  ```
  * Yes:
  ```erlang
  do_x() ->
      lists:foldl(fun(F, Acc) -> F(Acc) end, do_x(), [fun do_y/1, do_z/1]).
  ```

Some additional guidance can be found:
* http://www.erlang.se/doc/programming_rules.shtml
* https://www.youtube.com/watch?v=CQyt9Vlkbis

Some in-progress work is using [Elvis](https://github.com/inaka/elvis) to enforce style requirements so you can get feedback automatically for where your code has lost its way. This will eventually be added to Travis as well.

## Testing Your Code

You must meet the testing standards listed in the “Testing / Verification Standards” section (below)

All code commits require testing and evidence that tests were performed.

## Commits and Commit Messages

Please have git setup with consistent user information for each commit in a pull requests.  Preferably with your real name and a working email address (such as the one you use on the [2600hz-dev mailing list](https://groups.google.com/forum/#!forum/2600hz-dev)).

For quick reference, here are the relevant git commands:

```shell
git config --global user.name "Your Name Comes Here"
git config --global user.email you@yourdomain.example.com
```

Make separate commits for separate changes. If you cannot describe what the commit does in one sentence, it is probably a mix of changes and should be separated into several commits.

A well-crafted git commit message is the best way to communicate context about a change to fellow developers (and indeed to their future selves). A diff will tell you what changed, but only the commit message can properly tell you why.

### General Guidelines

Here are some general guidelines for submitting new code.

* Follow the guidelines for writing good commit messages.
* Do not commit commented-out code or files that are no longer needed.
* Check for unnecessary whitespace before committing with git diff --check.
* All pull requests should be issued to resolve a Jira ticket.  The branch name of the submission should be Jira ticket ID, such as KAZOO-42.
* Ensure that each pull request addresses only the work related to the ticket it is resolving.
* Keep the changes in a pull request as concise as possible.  If the pull request is a bug fix it should be the simplest possible approach with minimal code changes.
* All work should be rebased to the originating branch prior to pull request submission.

### Bug Fix Guidelines

Bug fixes must never introduce new features.

Bug fix branches must have originated from an updated version branch in the project which matches the affects version of the bug report.  The pull request should be made against the version branch and all subsequent version branches including master.

All non-bug fix work branches must have originated from an updated master branch in the project and the pull request must be issued against the master branch.

If the pull request was not for the master branch then all subsequent version branches must also have a pull request opened with the bug fix.  It is acceptable to work with 2600Hz to organize a sequence of pull requests when more convenient (such as refactoring the pull request against 3.22 and re-opening it for master).

A test should also be submitted to ensure that the bug is not accidentally reintroduced in the future.

### New Functionality/Features Guidelines

If you are implementing a new feature, also write new test cases.  The primary reason for writing test cases is not to prove that the new feature works correctly, but to make sure that it will be noticed if future changes — perhaps to code that seems unrelated — break the feature.

If you are implementing a new feature, also update the documentation to describe the feature.  If the documentation belongs to a missing document, such as an addition to an undocumented API it is acceptable to only document the addition.  It is nice to create the full missing documentation section ;)

Make sure the patch does not break backward compatibility. In general, we only break backward compatibility in major releases and only for a very good reason and usually after first deprecating the feature one or two releases beforehand.

If you must submit an incompatible change we prefer that the incompatible behaviour require an admin to enable the new functionality via a configuration parameter.

### New Applications Guidelines

TODO
MR: Thoughts: naming, copyrights section, support contacts that should work, !!!security testing!!!

### Core Updates/Modules Guidelines

TODO
MR: Thoughts: place where stuff goes (wapi, documents, deps, etc etc and how it should be broken down), comments, tests, support obligations

## Be Ego-less

### Understand and accept that you will make mistakes.

The point is to find them early, before they make it into production. Fortunately, except for the few of us developing rocket guidance software at JPL, mistakes are rarely fatal in our industry, so we can, and should, learn, laugh, and move on.

### You are not your code.

Remember that the entire point of a review is to find problems, and problems will be found. Don't take it personally when one is uncovered.

### No matter how much "karate" you know, someone else will always know more.

Such an individual can teach you some new moves if you ask. Seek and accept input from others, especially when you think it's not needed.

### Don't rewrite code without consultation.

There's a fine line between "fixing code" and "rewriting code." Know the difference, and pursue stylistic changes within the framework of a code review, not as a lone enforcer. Don’t use a sledgehammer on a problem to try and drive in a finishing nail - if code isn’t doing what you want and you don’t know how it’s supposed to work, ask first.

### Treat people who know less than you with respect, deference, and patience.

Nontechnical people who deal with developers on a regular basis almost universally hold the opinion that we are prima donnas at best and crybabies at worst. Don't reinforce this stereotype with anger and impatience.

### The only constant in the world is change.

Be open to it and accept it with a smile. Look at each change to your requirements, platform, or tool as a new challenge, not as some serious inconvenience to be fought.

### The only true authority stems from knowledge, not from position.

Knowledge engenders authority, and authority engenders respect – so if you want respect in an ego-less environment, cultivate knowledge.

### Fight for what you believe, but gracefully accept defeat.

Understand that sometimes your ideas will be overruled. Even if you do turn out to be right, don't take revenge or say, "I told you so" more than a few times at most, and don't make your dearly departed idea a martyr or rallying cry.

### Don't be "the guy in the room."

Don't be the guy coding in the dark office emerging only to buy cola. The guy in the room is out of touch, out of sight, and out of control and has no place in an open, collaborative environment.

### Be kind to the coder, not to the code.

As much as possible, make all of your comments positive and oriented to improving the code. Relate comments to local standards, program specs, increased performance, etc.

MR: Always think about security; think twice when deciding to hard-code something that can be different in other regions/countries;

### Hold 2600Hz Accountable

These are the standards that we set to ourselves as well as our partners.  As we push and the demands grow it is easy to forget some of these points in favor of speed or agility.  Occasional slips are marginally acceptable but we need to hold all those involved to the same standard.  If you see us not adhering to these guidelines call us out on it!  We welcome productive criticism!
