# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased
### Added
1. `chronometrist-third`, an extension to add support for the [Third Time](https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work) system.
2. New custom variable `chronometrist-key-value-preset-alist`, to define completion suggestions in advance.
3. New custom variable `chronometrist-key-value-use-database-history`, to control whether database history is used for key-value suggestions.
4. New commands `chronometrist-details-next-range` and `chronometrist-details-previous-range` to scroll through data in `chronometrist-details` buffers.

## [0.10.0] - 2022-02-15
### Changed
1. The value of `chronometrist-file` must now be a file path _without extension._ Please update your configurations.
2. The existing file format used by Chronometrist is now called the `plist` format.
3. The extension for files in the `plist` format is now `.plist`. Update the extension of your file to use it with the `plist` backend.

### Added
1. Multiple backend support - new custom variable `chronometrist-active-backend` to determine active backend, new command `chronometrist-switch-backend` to temporarily select a backend (with completion).
2. New `plist-group` backend, reducing time taken in startup and after changes to the file.
3. Unified migration interface with command `chronometrist-migrate`.
4. New custom variable `chronometrist-task-list`, to add/hide tasks without modifying the database. Setting it also disables generation of the task list from the database, speeding up many operations.
5. New command `chronometrist-discard-active`, to discard the active interval.
6. Debug logging messages - to view them, set `chronometrist-debug-enable`.

### Fixed
1. Code to detect the type of change made to the file has been rewritten, hopefully fixing some uncommon `read` errors and `args out of range` errors.

### Deprecated
1. The plist backend is deprecated and may be removed in a future release. The `plist-group` backend is more performant and extensible - please use `chronometrist-migrate` to convert your data to the `plist-group` backend.

## [0.9.0] - 2021-07-08
### Added
1. New commands `chronometrist-restart-task`, `chronometrist-extend-task`
2. Menus for `chronometrist`, `chronometrist-key-values`, and `chronometrist-details`
3. Custom ranges and filters for `chronometrist-details`. See command `chronometrist-details-set-range` and `chronometrist-details-set-filter`.
### Changed
4. Display graph ranges in `chronometrist-spark` column
5. `chronometrist-tags-add` and `chronometrist-key-values-unified-prompt` now also work interactively.

## [0.8.1] - 2021-06-01
### Changed
1. Distribute a tangled Elisp file as well as the literate program. Autoloads now work as usual.

## [0.8.0] - 2021-05-31
### Added
1. New frontend `chronometrist-details`, to display time intervals in a tabular format.
2. New extension `chronometrist-spark`, to display sparklines in the chronometrist buffer.
3. `chronometrist-schema`, custom variable used to define `tabulated-list-format`
### Changed
4. Renames -
   * `chronometrist-list-format-transformers` → `chronometrist-schema-transformers`
   * `chronometrist-entry-transformers` → `chronometrist-row-transformers` 
5. Hooks now use `defcustom` instead of `defvar`.
### Fixed
6. error when launching `chronometrist-statistics`

## [0.7.2] - 2021-05-18
### Changed
* If `chronometrist-file` is being edited, `chronometrist-timer` will not refresh the buffer or run `chronometrist-timer-hook`, preventing errors resulting from `read`ing of incomplete data and easing editing of the file.

## [0.7.1] - 2021-05-14
### Added
* key-values - `chronometrist-key-values-unified-prompt`, which uses `completing-read`. A more streamlined way to enter key-values, for those comfortable with entering/editing s-expressions directly.
### Removed
* key-values - all `choice.el`-based prompts have been removed.

## [0.7.0] - 2021-05-07
### Added
* Single key prompts for key-values - `chronometrist-tag-choice`, and `chronometrist-key-values-unified-choice`, with more to come.
* `chronometrist-reset`, to clear all internal state
### Changed
* `chronometrist` is now a literate Org program.
### Removed
* `chronometrist-skip-query-prompt`, `chronometrist-skip-query-reset`, and `chronometrist--skip-detail-prompts` - these are covered by the new single key prompt functions

## [0.6.5] - 2021-02-11
### Added
* Major mode (syntax highlighting, hook) for Chronometrist s-expression files, derived from emacs-lisp-mode
### Fixed
* Bug in updating the task list in some cases of the latest interval being modified.

## [0.6.4] - 2021-01-27
### Fixed
* Incorrect order of tag/key/value history
* Error when running emacs with `-q`
* `chronometrist` buffer not being updated when a new task is added or the sole interval for a task is removed.

## [0.6.3] - 2021-01-19
### Fixed
* Error from missing `require` form

## [0.6.2] - 2021-01-07
### Fixed
* Incorrect tag history in some situations

## [0.6.1] - 2021-01-04
### Fixed
* Removed a debugging `message` call which was accidentally left in

## [0.6.0] - 2021-01-04
### Added
* New hooks - `chronometrist-mode-hook`, `chronometrist-list-format-transformers`, `chronometrist-entry-transformers`, and `chronometrist-timer-hook`.
* New custom variable `chronometrist-sexp-pretty-print-function`
### Changed
* `chronometrist-plist-pp` now indents recursively.
### Optimization
* Refresh time after changing `chronometrist-file` is now near-instant for the most common situations - an expression being added to the end of the file, or the last expression in the file being changed or removed. This works for changes made by the user as well as changes made by Chronometrist (or other) commands.
* Tag, key, and value histories are generated before the user is prompted, rather than each time the file is saved.
### Fixed
* Remove quotes from key-value prompt in quit keybindings
* Lisp objects being stored as un`read`able strings in `chronometrist-value-history`, resulting in value suggestions not matching user input.
* `chronometrist-report` no longer calls `delete-other-windows`; use `chronometrist-report-mode-hook` if it is desired.
* Fixed infinite loop in `chronometrist-report` triggered by non-English locales.

## [0.5.6] - 2020-12-22
### Fixed
* Bug triggered by clocking out of a task with a name longer than the field width, caused by hacky way of determining the task at point. (issue #2)

## [0.5.5] - 2020-09-02
### Added
* `chronometrist-skip-query-prompt` to re-use last-used tags/key-values with a single key. (...assuming you use `y-or-n-p`)
### Changed
* Prompts for keys and values now use `completing-read`, making the interface and the controls more consistent.

## [0.5.4] - 2020-07-19
### Fixed
* Bug resulting in only the last tag combination being suggested

## [0.5.3] - 2020-07-06
### Changed
* `chronometrist-goals` has been renamed to `chronometrist-goal`

## [0.5.2] - 2020-07-05
### Fixed
* Package long description in the package menu

## [0.5.1] - 2020-06-30
### Fixed
* Error when adding task (trying to append an atom to a list)

## [0.5.0] - 2020-06-30
### Added
* Support for time goals via optional package `chronometrist-targets`.
* New hook - `chronometrist-file-change-hook`
### Changed
* Use [ts.el](https://github.com/alphapapa/ts.el) structs to represent date-time, wherever possible. (`chronometrist-events` and `chronometrist-file` being notable exceptions)
### Fixed
* Prefix arguments now work with the point on a button, too.
* Bug with missing entries in `chronometrist-key-history`
* Operations for adding a new s-expression and replacing the last s-expression have been optimized. Notably, commands for clocking in/out are now significantly faster.

## [0.4.4]
### Fixed
* Error when adding a task for the first time.

## [0.4.3] - 2020-05-03
### Changed
* `chronometrist-toggle-task-no-reason` (which did nothing since the migration from timeclock.el) is now called `chronometrist-toggle-task-no-hooks`. It will toggle the task without running the before-in/after-in/before-out/after-out functions.
### Fixed
* Refresh buffer when clocking in (instead of waiting for first timer refresh)
* Insertion of values is now slightly smarter about detecting and handling Lisp data types.

## [0.4.2] - 2020-01-15
### Fixed
* Library headers for MELPA release

## [0.4.1] - 2020-01-12
### Fixed
* Various declarations for MELPA release

## [0.4.0] - 2019-11-29
### Added
* Custom variable `chronometrist-activity-indicator` to change how an active task is indicated.
* `chronometrist-query-stop` for prompting on exiting Emacs
### Fixed
* `chronometrist-kv-accept` will not modify the file if there are no key-values.
* Regression in `chronometrist-value-history-populate`
* Migrate `chronometrist-statistics` to new format

## [0.3.2] - 2019-11-23
### Fixed
* Regression in `chronometrist-value-history-populate`

## [0.3.1] - 2019-11-22
### Fixed
* Improved load time via code cleanup + inhibiting `chronometrist-events-populate` for task start/stop

### Removed
1. Deprecated functions
2. Leftover pre-v0.3 variables

## [0.3.0] - 2019-10-31
### Added
* s-expression file format support
* functions to read arbitrary key-values (see `chronometrist-kv-add`, `chronometrist-kv-accept`, `chronometrist-kv-reject`)
* hook `chronometrist-after-in-functions`

### Changed
* hooks are now called `chronometrist-before-in-functions`, `chronometrist-before-out-functions`, and `chronometrist-after-out-functions`

### Removed
* timeclock.el file format support

### Deprecated
1. `chronometrist-timestamp->seconds`
2. `chronometrist-timestamp-list->seconds`
3. `chronometrist-time-re-file`
4. `chronometrist-get-end-time`
5. `chronometrist-date-op-internal`
6. `chronometrist-reason-list`
7. `chronometrist-ask-for-reason`

## [0.2.2] - 2019-09-09
### Fixed
* Error resulting from incorrect variable name in chronometrist-maybe-stop-timer
* Long waiting times after saving timeclock-file due to multiple, erroneously-created file system watchers.

### Deprecated
* timeclock will be removed as a backend in the next release.

## [0.2.1] - 2019-08-14
### Fixed
* bug with wrongly named function in chronometrist-report

## [0.2.0] - 2019-08-09
### Added
* Autoload cookies
* chronometrist-before-project-stop-functions hook

### Fixed
* Try to remove unnecessary file re-reading
* Buffer refresh bugs

## [0.1.0] - 2019-08-05
