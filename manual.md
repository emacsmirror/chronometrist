
# Table of Contents

1.  [Benefits](#benefits)
2.  [Limitations](#limitations)
3.  [Comparisons](#comparisons)
    1.  [timeclock.el](#timeclock.el)
    2.  [Org time tracking](#org-time-tracking)
4.  [Installation](#installation)
    1.  [from MELPA](#install-from-melpa)
    2.  [from Git](#install-from-git)
5.  [Usage](#usage)
    1.  [chronometrist](#usage-chronometrist)
    2.  [chronometrist-report](#usage-chronometrist-report)
    3.  [chronometrist-statistics](#usage-chronometrist-statistics)
    4.  [chronometrist-details](#org6ee0fb7)
    5.  [common commands](#usage-common-commands)
    6.  [Time goals/targets](#time-goals)
6.  [How-to](#how-to)
    1.  [How to display a prompt when exiting with an active task](#how-to-prompt-when-exiting-emacs)
    2.  [How to load the program using literate-elisp](#how-to-literate-elisp)
    3.  [How to attach tags to time intervals](#how-to-tags)
    4.  [How to attach key-values to time intervals](#how-to-key-value-pairs)
    5.  [How to skip running hooks/attaching tags and key values](#orgfbe4680)
    6.  [How to open certain files when you start a task](#how-to-open-files-on-task-start)
    7.  [How to warn yourself about uncommitted changes](#how-to-warn-uncommitted-changes)
    8.  [How to display the current time interval in the activity indicator](#how-to-activity-indicator)
    9.  [How to back up your Chronometrist data](#how-to-backup)
    10. [How to configure Vertico for use with Chronometrist](#howto-vertico)
7.  [Explanation](#org25cd1c5)
    1.  [Literate Program](#explanation-literate-program)
8.  [User's reference](#org5f69f60)
9.  [Contributions and contact](#contributions-contact)
10. [License](#license)
11. [Thanks](#thanks)

<a href="https://liberapay.com/contrapunctus/donate">
  <img alt="Donate using Liberapay" src="https://img.shields.io/liberapay/receives/contrapunctus.svg?logo=liberapay">
</a>

<a href="https://melpa.org/#/chronometrist">
  <img src="https://melpa.org/packages/chronometrist-badge.svg">
</a>

Chronometrist is a friendly and powerful personal time tracker and analyzer for Emacs.

![img](doc/2022-02-20 13-26-53.png "Screenshot of the main Chronometrist buffer, with the enabled extensions [chronometrist-goal](#time-goals) ("Targets" column + alerts) and chronometrist-spark ("Graph" column displaying the activity for the past 4 weeks).")


<a id="benefits"></a>

# Benefits

1.  Extremely simple and efficient to use
2.  Displays useful information about your time usage
3.  Support for both mouse and keyboard
4.  Human errors in tracking are easily fixed by editing a plain text file
5.  Hooks to let you perform arbitrary actions when starting/stopping tasks
6.  Fancy graphs with the `chronometrist-spark` extension


<a id="limitations"></a>

# Limitations

1.  No support for concurrent tasks.


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

You can get `chronometrist` from <https://tildegit.org/contrapunctus/chronometrist> or <https://codeberg.org/contrapunctus/chronometrist>

`chronometrist` requires

-   Emacs v25 or higher
-   [dash.el](https://github.com/magnars/dash.el)
-   [ts.el](https://github.com/alphapapa/ts.el)

Add the "elisp/" subdirectory to your load-path, and `(require 'chronometrist)`.


<a id="usage"></a>

# Usage


<a id="usage-chronometrist"></a>

## chronometrist

Run `M-x chronometrist` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Click or hit `RET` (`chronometrist-toggle-task`) on a project to start tracking time for it. If it's already clocked in, it will be clocked out.

You can also hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. =C-1 RET= will toggle the project with index 1.

Press `r` to see a weekly report (see `chronometrist-report`)


<a id="usage-chronometrist-report"></a>

## chronometrist-report

Run `M-x chronometrist-report` (or `chronometrist` with a prefix argument of 1, or press `r` in the `chronometrist` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.


<a id="usage-chronometrist-statistics"></a>

## chronometrist-statistics

Run `M-x chronometrist-statistics` (or `chronometrist` with a prefix argument of 2) to view statistics.

Press `b` to look at past time ranges, and `f` for future ones.


<a id="org6ee0fb7"></a>

## chronometrist-details


<a id="usage-common-commands"></a>

## common commands

In the buffers created by the previous three commands, you can press `l` (`chronometrist-open-log`) to view/edit your `chronometrist-file`, which by default is `~/.emacs.d/chronometrist.sexp`.

All of these commands will kill their buffer when run again with the buffer visible, so the keys you bind them to behave as a toggle.

All buffers keep themselves updated via an idle timer - no need to frequently press `g` to update.


<a id="time-goals"></a>

## Time goals/targets

If you wish you could define time goals for some tasks, and have Chronometrist notify you when you're approaching the goal, completing it, or exceeding it, check out the extension [chronometrist-goal.el](https://github.com/contrapunctus-1/chronometrist-goal/).


<a id="how-to"></a>

# How-to

See the Customize groups `chronometrist` and `chronometrist-report` for variables intended to be user-customizable.


<a id="how-to-prompt-when-exiting-emacs"></a>

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

1.  Add `chronometrist-tags-add` to one or more of these hooks <sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> -

        (add-to-list 'chronometrist-after-in-functions 'chronometrist-tags-add)
        (add-to-list 'chronometrist-before-out-functions 'chronometrist-tags-add)
        (add-to-list 'chronometrist-after-out-functions 'chronometrist-tags-add)
2.  clock in/clock out to trigger the hook.

    The prompt suggests past combinations you used for the current task, which you can browse with `M-p=/=M-n`. You can leave it blank by pressing `RET`.


<a id="how-to-key-value-pairs"></a>

## How to attach key-values to time intervals

1.  Add `chronometrist-kv-add` to one or more of these hooks <sup><a id="fnr.1.100" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> -

        (add-to-list 'chronometrist-after-in-functions 'chronometrist-kv-add)
        (add-to-list 'chronometrist-before-out-functions 'chronometrist-kv-add)
        (add-to-list 'chronometrist-after-out-functions 'chronometrist-kv-add)

To exit the prompt, press the key it indicates for quitting - you can then edit the resulting key-values by hand if required. Press `C-c C-c` to accept the key-values, or `C-c C-k` to cancel.


<a id="orgfbe4680"></a>

## How to skip running hooks/attaching tags and key values

Use `M-RET` (`chronometrist-toggle-task-no-hooks`) to clock in/out.


<a id="how-to-open-files-on-task-start"></a>

## How to open certain files when you start a task

An idea from the author's own init -

    (defun my-start-project (project)
      (pcase project
        ("Guitar"
         (find-file-other-window "~/repertoire.org"))
        ;; ...
        ))

    (add-hook 'chronometrist-before-in-functions 'my-start-project)


<a id="how-to-warn-uncommitted-changes"></a>

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


<a id="how-to-activity-indicator"></a>

## How to display the current time interval in the activity indicator

    (defun my-activity-indicator ()
      (--> (chronometrist-latest-record (chronometrist-active-backend))
           (plist-put it :stop (chronometrist-format-time-iso8601))
           (list it)
           (chronometrist-events-to-durations it)
           (-reduce #'+ it)
           (truncate it)
           (chronometrist-format-duration it)))

    (setq chronometrist-activity-indicator #'my-activity-indicator)


<a id="how-to-backup"></a>

## How to back up your Chronometrist data

I suggest backing up Chronometrist data on each save using the [async-backup](https://tildegit.org/contrapunctus/async-backup) package.<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> Here's how you can do that.

1.  Add the following to your init.

        (use-package async-backup)
2.  Open your Chronometrist file and add `async-backup` to a buffer-local `after-save-hook`.

        M-x chronometrist-open-log
        M-x add-file-local-variable-prop-line RET eval RET (add-hook 'after-save-hook #'async-backup nil t) RET
3.  Optionally, configure `backup-directory-alist` to set a specific directory for the backups.


<a id="howto-vertico"></a>

## How to configure Vertico for use with Chronometrist

By default, [Vertico](https://github.com/minad/vertico) uses its own sorting function - for some commands (such as `chronometrist-key-values-unified-prompt`) this results in *worse* suggestions, since Chronometrist sorts suggestions in most-recent-first order.

You can either disable Vertico's sorting entirely -

    (setq vertico-sort-function nil)

Or use `vertico-multiform` to disable sorting for only specific commands -

    (use-package vertico-multiform
      :init (vertico-multiform-mode)
      :config
      (setq vertico-multiform-commands
            '((chronometrist-toggle-task          (vertico-sort-function . nil))
              (chronometrist-toggle-task-no-hooks (vertico-sort-function . nil))
              (chronometrist-key-values-unified-prompt      (vertico-sort-function . nil)))))


<a id="org25cd1c5"></a>

# Explanation


<a id="explanation-literate-program"></a>

## Literate Program

Chronometrist is a literate program, made using Org - the canonical source is the `chronometrist.org` file, which contains source blocks. These are provided to users after *tangling* (extracting the source into an Emacs Lisp file).

The Org file can also be loaded directly using the [literate-elisp](https://github.com/jingtaozf/literate-elisp) package, so that all source links (e.g. `xref`, `describe-function`) lead to the Org file, within the context of the concerned documentation. See [How to load the program using literate-elisp](#how-to-literate-elisp).

`chronometrist.org` is also included in MELPA installs, although not used directly by default, since doing so would interfere with automatic generation of autoloads.


<a id="org5f69f60"></a>

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


<a id="contributions-contact"></a>

# Contributions and contact

Feedback and MRs are very welcome. 🙂

-   <TODO.md> has a long list of tasks
-   <elisp/chronometrist.md> contains all developer-oriented documentation

If you have tried using Chronometrist, I'd love to hear your experiences! Get in touch with the author and other Emacs users in the Emacs channel on the Jabber network - [xmpp:emacs@salas.suchat.org?join](https://conversations.im/j/emacs@salas.suchat.org) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))


<a id="license"></a>

# License

I dream of a world where all software is liberated - transparent, trustable, and accessible for anyone to use or improve. But I don't want to make demands or threats (e.g. via legal conditions) to get there.

I'd rather make a request - please do everything you can to help that dream come true. Please Unlicense as much software as you can.

Chronometrist is released under your choice of [Unlicense](https://unlicense.org/) or the [WTFPL](http://www.wtfpl.net/).

(See files [UNLICENSE](UNLICENSE) and [WTFPL](WTFPL)).


<a id="thanks"></a>

# Thanks

The main buffer and the report buffer are copied from the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

wasamasa, bpalmer, aidalgol, pjb and the rest of #emacs for their tireless help and support

jwiegley for `timeclock.el`, which we used as a backend in earlier versions

blandest for helping me with the name

fiete and wu-lee for testing and bug reports


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> but not `chronometrist-before-in-functions`

<sup><a id="fn.2" href="#fnr.2">2</a></sup> It is possible to use Emacs' built-in backup system to do it, but since it is synchronous, doing so will greatly slow down saving of the Chronometrist file.
