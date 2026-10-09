# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Tests
- **The test suite loads and runs again.** It had not compiled since the
  verbs were renamed with a leading dot (commit 9c62711): the test package
  `:use`d both FiveAM and cl-dplyr, which both export `!` (package
  conflict), and every test still called `filter`, `mutate`, `pull`, ...
  instead of `.filter`, `.mutate`, `.pull`. The test package now
  shadowing-imports `cl-dplyr:!` and the tests call the dotted verbs.
  Result: 64/67 checks pass; the 3 failures are genuine bugs (`t` in
  `case-when`, `.desc` in `arrange`, join columns), fixed in the next
  commits (topic: test-suite repair).
