
# Table of Contents

1.  [Benefits](#org83c3199)
2.  [Limitations](#org265ad3f)
3.  [Comparisons](#comparisons)
    1.  [timeclock.el](#timeclock.el)
    2.  [Org time tracking](#org-time-tracking)
4.  [Installation](#installation)
    1.  [from MELPA](#install-from-melpa)
    2.  [from Git](#install-from-git)
5.  [Usage](#usage)
    1.  [chronometrist](#chronometrist-1)
    2.  [chronometrist-report](#chronometrist-report)
    3.  [chronometrist-statistics](#chronometrist-statistics)
    4.  [chronometrist-details](#org0ec335b)
    5.  [common commands](#org170f814)
    6.  [Time goals/targets](#time-goalstargets)
6.  [How-to](#customization)
    1.  [How to display a prompt when exiting with an active task](#prompt-when-exiting-emacs)
    2.  [How to load the program using literate-elisp](#how-to-literate-elisp)
    3.  [How to attach tags to time intervals](#how-to-tags)
    4.  [How to attach key-values to time intervals](#how-to-key-value-pairs)
    5.  [How to skip running hooks/attaching tags and key values](#org7fd0f91)
    6.  [How to open certain files when you start a task](#open-certain-files-when-you-start-a-task)
    7.  [How to warn yourself about uncommitted changes](#uncommitted-changes)
    8.  [How to display the current time interval in the activity indicator](#current-time-interval-in-activity-indicator)
7.  [Explanation](#orga5e2e2e)
    1.  [Literate Program](#explanation-literate-program)
8.  [User's reference](#org2c64088)
9.  [Contributions and contact](#contributions-and-contact)
10. [License](#license)
11. [Thanks](#thanks)

<a href="https://liberapay.com/contrapunctus/donate"><img alt="Donate using Liberapay" src="https://img.shields.io/liberapay/receives/contrapunctus.svg?logo=liberapay"></a>

<a href="https://melpa.org/#/chronometrist"><img src="https://melpa.org/packages/chronometrist-badge.svg"></a>

A time tracker in Emacs with a nice interface

Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)


<a id="org83c3199"></a>

# Benefits

1.  Extremely simple and efficient to use
2.  Displays useful information about your time usage
3.  Support for both mouse and keyboard
4.  Human errors in tracking are easily fixed by editing a plain text file
5.  Hooks to let you perform arbitrary actions when starting/stopping tasks
6.  Fancy graphs with chronometrist-sparkline extension


<a id="org265ad3f"></a>

# Limitations

1.  No support (yet) for adding a task without clocking into it.
2.  No support for concurrent tasks.


<a id="comparisons"></a>

# Comparisons


<a id="timeclock.el"></a>

## timeclock.el

Compared to timeclock.el, Chronometrist

-   stores data in an s-expression format rather than a line-based one
-   supports attaching tags and arbitrary key-values to time intervals
-   has commands to shows useful summaries
-   has more hooks


<a id="org-time-tracking"></a>

## Org time tracking

Chronometrist and Org time tracking seem to be equivalent in terms of capabilities, approaching the same ends through different means.

-   Chronometrist doesn't have a mode line indicator at the moment. (planned)
-   Chronometrist doesn't have Org's sophisticated querying facilities. (an SQLite backend is planned)
-   Org does so many things that keybindings seem to necessarily get longer. Chronometrist has far fewer commands than Org, so most of the keybindings are single keys, without modifiers.
-   Chronometrist's UI makes keybindings discoverable - they are displayed in the buffers themselves.
-   Chronometrist's UI is cleaner, since the storage is separate from the display. It doesn't show tasks as trees like Org, but it uses tags and key-values to achieve that. Additionally, navigating a flat list takes fewer user operations than navigating a tree.
-   Chronometrist data is just s-expressions (plists), and may be easier to parse than a complex text format with numerous use-cases.


<a id="installation"></a>

# Installation


<a id="install-from-melpa"></a>

## from MELPA

1.  Set up MELPA - <https://melpa.org/#/getting-started>
    
    (Chronometrist uses Semantic Versioning and the developer is accident-prone, so using MELPA Stable is suggested 😏)
2.  `M-x package-install RET chronometrist RET`


<a id="install-from-git"></a>

## from Git

You can get `chronometrist` from <https://tildegit.org/contrapunctus/chronometrist>

`chronometrist` requires

-   Emacs v25 or higher
-   [dash.el](https://github.com/magnars/dash.el)
-   [ts.el](https://github.com/alphapapa/ts.el)

Add the "elisp/" subdirectory to your load-path, and `(require 'chronometrist)`.


<a id="usage"></a>

# Usage


<a id="chronometrist-1"></a>

## chronometrist

Run `M-x chronometrist` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Click or hit `RET` (`chronometrist-toggle-task`) on a project to start tracking time for it. If it's already clocked in, it will be clocked out.

You can also hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. =C-1 RET= will toggle the project with index 1.

Press `r` to see a weekly report (see `chronometrist-report`)


<a id="chronometrist-report"></a>

## chronometrist-report

Run `M-x chronometrist-report` (or `chronometrist` with a prefix argument of 1, or press `r` in the `chronometrist` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.


<a id="chronometrist-statistics"></a>

## chronometrist-statistics

Run `M-x chronometrist-statistics` (or `chronometrist` with a prefix argument of 2) to view statistics.

Press `b` to look at past time ranges, and `f` for future ones.


<a id="org0ec335b"></a>

## chronometrist-details


<a id="org170f814"></a>

## common commands

In the buffers created by the previous three commands, you can press `l` (`chronometrist-open-log`) to view/edit your `chronometrist-file`, which by default is `~/.emacs.d/chronometrist.sexp`.

All of these commands will kill their buffer when run again with the buffer visible, so the keys you bind them to behave as a toggle.

All buffers keep themselves updated via an idle timer - no need to frequently press `g` to update.


<a id="time-goalstargets"></a>

## Time goals/targets

If you wish you could define time goals for some tasks, and have Chronometrist notify you when you're approaching the goal, completing it, or exceeding it, check out the extension [chronometrist-goal.el](https://github.com/contrapunctus-1/chronometrist-goal/).


<a id="customization"></a>

# How-to

See the Customize groups `chronometrist` and `chronometrist-report` for variables intended to be user-customizable.


<a id="prompt-when-exiting-emacs"></a>

## How to display a prompt when exiting with an active task

Evaluate or add to your init.el the following -
`(add-hook 'kill-emacs-query-functions 'chronometrist-query-stop)`


<a id="how-to-literate-elisp"></a>

## How to load the program using literate-elisp

    (add-to-list 'load-path "<directory containing chronometrist.org>")
    
    (require 'literate-elisp) ;; or autoload, use-package, ...
    (literate-elisp-load "chronometrist.org")


<a id="how-to-tags"></a>

## How to attach tags to time intervals

1.  Add `chronometrist-tags-add` to one or more of these hooks <sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> -
    
        (add-to-list 'chronometrist-after-in-functions 'chronometrist-tags-add)
        (add-to-list 'chronometrist-before-out-functions 'chronometrist-tags-add)
        (add-to-list 'chronometrist-after-out-functions 'chronometrist-tags-add)
2.  clock in/clock out to trigger the hook.
    
    The prompt suggests past combinations you used for the current task, which you can browse with `M-p=/=M-n`. You can leave it blank by pressing `RET`.


<a id="how-to-key-value-pairs"></a>

## How to attach key-values to time intervals

1.  Add `chronometrist-kv-add` to one or more of these hooks <sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup> -

    (add-to-list 'chronometrist-after-in-functions 'chronometrist-kv-add)
    (add-to-list 'chronometrist-before-out-functions 'chronometrist-kv-add)
    (add-to-list 'chronometrist-after-out-functions 'chronometrist-kv-add)

To exit the prompt, press the key it indicates for quitting - you can then edit the resulting key-values by hand if required. Press `C-c C-c` to accept the key-values, or `C-c C-k` to cancel.


<a id="org7fd0f91"></a>

## How to skip running hooks/attaching tags and key values

Use `M-RET` (`chronometrist-toggle-task-no-hooks`) to clock in/out.


<a id="open-certain-files-when-you-start-a-task"></a>

## How to open certain files when you start a task

An idea from the author's own init -

    (defun my-start-project (project)
      (pcase project
        ("Guitar"
         (find-file-other-window "~/repertoire.org"))
        ;; ...
        ))
    
    (add-hook 'chronometrist-before-in-functions 'my-start-project)


<a id="uncommitted-changes"></a>

## How to warn yourself about uncommitted changes

Another one, prompting the user if they have uncommitted changes in a git repository (assuming they use [Magit](https://magit.vc/)) -

    (autoload 'magit-anything-modified-p "magit")
    
    (defun my-commit-prompt ()
      "Prompt user if `default-directory' is a dirty Git repository.
    Return t if the user answers yes, if the repository is clean, or
    if there is no Git repository.
    
    Return nil (and run `magit-status') if the user answers no."
      (cond ((not (magit-anything-modified-p)) t)
            ((yes-or-no-p
              (format "You have uncommitted changes in %S. Really clock out? "
                      default-directory)) t)
            (t (magit-status) nil)))
    
    (add-hook 'chronometrist-before-out-functions 'my-commit-prompt)


<a id="current-time-interval-in-activity-indicator"></a>

## How to display the current time interval in the activity indicator

    (defun my-activity-indicator ()
      (thread-last (plist-put (chronometrist-last)
                              :stop (chronometrist-format-time-iso8601))
        list
        chronometrist-events-to-durations
        (-reduce #'+)
        truncate
        chronometrist-format-time))
    
    (setq chronometrist-activity-indicator #'my-activity-indicator)


<a id="orga5e2e2e"></a>

# Explanation


<a id="explanation-literate-program"></a>

## Literate Program

Chronometrist is a literate program, made using Org - the canonical source is the `chronometrist.org` file, which contains source blocks. These are provided to users after *tangling* (extracting the source into an Emacs Lisp file).

The Org file can also be loaded directly using the   [literate-elisp](https://github.com/jingtaozf/literate-elisp) package, so that all source links (e.g. `xref`, `describe-function`) lead to the Org file, within the context of the concerned documentation. See [How to load the program using literate-elisp](#how-to-literate-elisp).

`chronometrist.org` is also included in MELPA installs, although not used directly by default, since doing so would interfere with automatic generation of autoloads.


<a id="org2c64088"></a>

# User's reference

All variables intended for user customization are listed here. They serve as the public API for this project for the purpose of semantic versioning. Any changes to these which require a user to modify their configuration are considered breaking changes.

1.  `chronometrist-file`
2.  `chronometrist-buffer-name`
3.  `chronometrist-report-buffer-name`
4.  `chronometrist-details-buffer-name`
5.  `chronometrist-sexp-pretty-print-function`
6.  `chronometrist-hide-cursor`
7.  `chronometrist-update-interval`
8.  `chronometrist-activity-indicator`

Buffer schemas

1.  `chronometrist-schema`
2.  `chronometrist-details-schema`

Hooks

1.  `chronometrist-mode-hook`
2.  `chronometrist-schema-transformers`
3.  `chronometrist-row-transformers`
4.  `chronometrist-before-in-functions`
5.  `chronometrist-after-in-functions`
6.  `chronometrist-before-out-functions`
7.  `chronometrist-after-out-functions`
8.  `chronometrist-file-change-hook`
9.  `chronometrist-timer-hook`


<a id="contributions-and-contact"></a>

# Contributions and contact

Feedback and MRs are very welcome. 🙂

-   <TODO.md> has a long list of tasks
-   <doc/manual.md> contains an overview of the codebase, explains various mechanisms and decisions, and has a reference of definitions.

If you have tried using Chronometrist, I'd love to hear your experiences! Get in touch with the author and other Emacs users in the Emacs channel on the Jabber network - [xmpp:emacs@salas.suchat.org?join](https://conversations.im/j/emacs@salas.suchat.org) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))


<a id="license"></a>

# License

I dream of a world where all software is liberated - transparent, trustable, and accessible for anyone to use or improve. But I don't want to make demands or threats (e.g. via legal conditions) to get there.

I'd rather make a request - please do everything you can to help that dream come true. Please Unlicense as much software as you can.

Chronometrist is released under your choice of [Unlicense](https://unlicense.org/) or the [WTFPL](http://www.wtfpl.net/).

(See files <UNLICENSE> and <WTFPL>).


<a id="thanks"></a>

# Thanks

wasamasa, bpalmer, aidalgol, pjb and the rest of #emacs for their tireless help and support

jwiegley for timeclock.el, which we used as a backend in earlier versions

blandest for helping me with the name

fiete and wu-lee for testing and bug reports


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> but not `chronometrist-before-in-functions`

<sup><a id="fn.2" href="#fnr.2">2</a></sup> but not `chronometrist-before-in-functions`