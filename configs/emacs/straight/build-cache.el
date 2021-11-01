
:tanat

"27.2"

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2021-11-01 11:59:18" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2021-11-01 11:59:18" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2021-11-01 11:59:18" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2021-11-01 11:59:18" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2021-11-01 11:59:18" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2021-11-01 11:59:18" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "embark" ("2021-11-01 11:59:20" ("emacs") (:type git :flavor melpa :files ("embark.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark" :package "embark" :local-repo "embark")) "embark-consult" ("2021-11-01 11:59:20" ("emacs" "embark" "consult") (:flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :package "embark-consult" :local-repo "embark" :type git :repo "oantolin/embark" :host github)) "consult" ("2021-11-01 11:59:20" ("emacs") (:type git :flavor melpa :host github :repo "minad/consult" :package "consult" :local-repo "consult")) "ace-window" ("2021-11-01 11:59:20" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "avy" ("2021-11-01 11:59:20" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "0x0" ("2021-11-01 11:59:20" ("emacs") (:type git :flavor melpa :host gitlab :repo "willvaughn/emacs-0x0" :package "0x0" :local-repo "emacs-0x0")) "zig-mode" ("2021-11-01 11:59:21" ("emacs") (:type git :flavor melpa :host github :repo "ziglang/zig-mode" :package "zig-mode" :local-repo "zig-mode"))))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight straight-x straight-autoloads) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight" '("straight-"))) (defvar straight-x-pinned-packages nil "List of pinned packages.") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight-x" '("straight-x-"))) (provide 'straight-autoloads)) "embark" ((embark embark-autoloads) (defun embark--record-this-command nil "Record command which opened the minibuffer.
We record this because it will be the default action.
This function is meant to be added to `minibuffer-setup-hook'." (setq-local embark--command this-command)) (add-hook 'minibuffer-setup-hook #'embark--record-this-command) (autoload 'embark-prefix-help-command "embark" "Prompt for and run a command bound in the prefix used to reach this command.
The prefix described consists of all but the last event of the
key sequence that ran this command.  This function is intended to
be used as a value for `prefix-help-command'.

In addition to using completion to select a command, you can also
type @ and the key binding (without the prefix)." t nil) (autoload 'embark-bindings "embark" "Explore all current command key bindings with `completing-read'.
The selected command will be executed.  The set of key bindings can
be restricted by passing a PREFIX key.

(fn &optional PREFIX)" t nil) (autoload 'embark-act "embark" "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target.

(fn &optional ARG)" t nil) (autoload 'embark-dwim "embark" "Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overidden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type.

See `embark-act' for the meaning of the prefix ARG.

(fn &optional ARG)" t nil) (autoload 'embark-become "embark" "Make current command become a different command.
Take the current minibuffer input as initial input for new
command.  The new command can be run normally using key bindings or
\\[execute-extended-command], but if the current command is found in a keymap in
`embark-become-keymaps', that keymap is activated to provide
convenient access to the other commands in it.

If FULL is non-nil (interactively, if called with a prefix
argument), the entire minibuffer contents are used as the initial
input of the new command.  By default only the part of the
minibuffer contents between the current completion boundaries is
taken.  What this means is fairly technical, but (1) usually
there is no difference: the completion boundaries include the
entire minibuffer contents, and (2) the most common case where
these notions differ is file completion, in which case the
completion boundaries single out the path component containing
point.

(fn &optional FULL)" t nil) (autoload 'embark-collect-live "embark" "Create a live-updating Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect Live\".

(fn &optional INITIAL-VIEW)" t nil) (autoload 'embark-collect-snapshot "embark" "Create an Embark Collect buffer.
Optionally start in INITIAL-VIEW (either `list' or `grid')
instead of what `embark-collect-initial-view-alist' specifies.
Interactively, \\[universal-argument] means grid view, a prefix
argument of 1 means list view.

To control the display, add an entry to `display-buffer-alist'
with key \"Embark Collect\".

(fn &optional INITIAL-VIEW)" t nil) (autoload 'embark-collect-completions "embark" "Create an ephemeral live-updating Embark Collect buffer." t nil) (autoload 'embark-collect-completions-after-delay "embark" "Start `embark-collect-live' after `embark-collect-live-initial-delay'.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup every time you use the minibuffer." nil nil) (autoload 'embark-collect-completions-after-input "embark" "Start `embark-collect-completions' after some minibuffer input.
Add this function to `minibuffer-setup-hook' to have an Embark
Live Collect buffer popup soon after you type something in the
minibuffer; the length of the delay after typing is given by
`embark-collect-live-initial-delay'." nil nil) (autoload 'embark-switch-to-collect-completions "embark" "Switch to the Embark Collect Completions buffer, creating it if necessary." t nil) (autoload 'embark-export "embark" "Create a type-specific buffer to manage current candidates.
The variable `embark-exporters-alist' controls how to make the
buffer for each type of completion." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "embark" '("embark-"))) (provide 'embark-autoloads)) "consult" ((consult-autoloads consult-icomplete consult-register consult-xref consult-org consult consult-selectrum consult-vertico consult-imenu consult-flymake consult-compile) (autoload 'consult-completion-in-region "consult" "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer

(fn START END COLLECTION &optional PREDICATE)" nil nil) (autoload 'consult-completing-read-multiple "consult" "Enhanced replacement for `completing-read-multiple'.
See `completing-read-multiple' for the documentation of the arguments.

(fn PROMPT TABLE &optional PRED REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil) (autoload 'consult-multi-occur "consult" "Improved version of `multi-occur' based on `completing-read-multiple'.

See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

(fn BUFS REGEXP &optional NLINES)" t nil) (autoload 'consult-outline "consult" "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate preview.
The symbol at point is added to the future history." t nil) (autoload 'consult-mark "consult" "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t nil) (autoload 'consult-global-mark "consult" "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t nil) (autoload 'consult-line "consult" "Search for a matching line.

Depending on the setting `consult-line-point-placement' the command jumps to
the beginning or the end of the first match on the line or the line beginning.
The default candidate is the non-empty line next to point. This command obeys
narrowing. Optional INITIAL input can be provided. The search starting point is
changed if the START prefix argument is set. The symbol at point and the last
`isearch-string' is added to the future history.

(fn &optional INITIAL START)" t nil) (autoload 'consult-line-multi "consult" "Search for a matching line in multiple buffers.

By default search across all project buffers. If the prefix argument QUERY is
non-nil, all buffers are searched. Optional INITIAL input can be provided. See
`consult-line' for more information. In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn QUERY &optional INITIAL)" t nil) (autoload 'consult-keep-lines "consult" "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. When
called from elisp, the filtering is performed by a FILTER function. This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn &optional FILTER INITIAL)" t nil) (autoload 'consult-focus-lines "consult" "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. With
optional prefix argument SHOW reveal the hidden lines. Alternatively the
command can be restarted to reveal the lines. When called from elisp, the
filtering is performed by a FILTER function. This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn &optional SHOW FILTER INITIAL)" t nil) (autoload 'consult-goto-line "consult" "Read line number and jump to the line with preview.

Jump directly if a line number is given as prefix ARG. The command respects
narrowing and the settings `consult-goto-line-numbers' and
`consult-line-numbers-widen'.

(fn &optional ARG)" t nil) (autoload 'consult-recent-file "consult" "Find recent file using `completing-read'." t nil) (autoload 'consult-file-externally "consult" "Open FILE externally using the default application of the system.

(fn FILE)" t nil) (autoload 'consult-mode-command "consult" "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

(fn &rest MODES)" t nil) (autoload 'consult-yank-from-kill-ring "consult" "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

(fn STRING &optional ARG)" t nil) (autoload 'consult-yank-pop "consult" "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

(fn &optional ARG)" t nil) (autoload 'consult-yank-replace "consult" "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28.

(fn STRING)" t nil) (autoload 'consult-bookmark "consult" "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration.

(fn NAME)" t nil) (autoload 'consult-apropos "consult" "Select pattern and call `apropos'.

The default value of the completion is the symbol at point. As a better
alternative, you can run `embark-export' from commands like `M-x' and
`describe-symbol'." t nil) (autoload 'consult-complex-command "consult" "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t nil) (autoload 'consult-history "consult" "Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument.

(fn &optional HISTORY)" t nil) (autoload 'consult-isearch-history "consult" "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t nil) (autoload 'consult-minor-mode-menu "consult" "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t nil) (autoload 'consult-theme "consult" "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

(fn THEME)" t nil) (autoload 'consult-buffer "consult" "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources." t nil) (autoload 'consult-buffer-other-window "consult" "Variant of `consult-buffer' which opens in other window." t nil) (autoload 'consult-buffer-other-frame "consult" "Variant of `consult-buffer' which opens in other frame." t nil) (autoload 'consult-kmacro "consult" "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

(fn ARG)" t nil) (autoload 'consult-grep "consult" "Search for regexp with grep in DIR with INITIAL input.

The input string is split, the first part of the string is passed to
the asynchronous grep process and the second part of the string is
passed to the completion-style filtering. The input string is split at
a punctuation character, which is given as the first character of the
input string. The format is similar to Perl-style regular expressions,
e.g., /regexp/. Furthermore command line options can be passed to
grep, specified behind --.

Example: #async-regexp -- grep-opts#filter-string

The symbol at point is added to the future history. If `consult-grep'
is called interactively with a prefix argument, the user can specify
the directory to search in. By default the project directory is used
if `consult-project-root-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-git-grep "consult" "Search for regexp with grep in DIR with INITIAL input.

See `consult-grep' for more details.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-ripgrep "consult" "Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-find "consult" "Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-locate "consult" "Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional INITIAL)" t nil) (autoload 'consult-man "consult" "Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional INITIAL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult" '("consult-"))) (autoload 'consult-compile-error "consult-compile" "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-compile" '("consult-compile--"))) (autoload 'consult-flymake "consult-flymake" "Jump to Flymake diagnostic." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-flymake" '("consult-flymake--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-icomplete" '("consult-icomplete--refresh"))) (autoload 'consult-imenu "consult-imenu" "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'." t nil) (autoload 'consult-imenu-multi "consult-imenu" "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details. In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn &optional QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-imenu" '("consult-imenu-"))) (autoload 'consult-org-heading "consult-org" "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

(fn &optional MATCH SCOPE)" t nil) (autoload 'consult-org-agenda "consult-org" "Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this.

(fn &optional MATCH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-org" '("consult-org--"))) (autoload 'consult-register-window "consult-register" "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

(fn BUFFER &optional SHOW-EMPTY)" nil nil) (autoload 'consult-register-format "consult-register" "Enhanced preview of register REG.

This function can be used as `register-preview-function'.

(fn REG)" nil nil) (autoload 'consult-register "consult-register" "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick access to
registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in built-in
register access functions. The command supports narrowing, see
`consult-register-narrow'. Marker positions are previewed. See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

(fn &optional ARG)" t nil) (autoload 'consult-register-load "consult-register" "Do what I mean with a REG.

For a window configuration, restore it. For a number or text, insert it. For a
location, jump to it. See `jump-to-register' and `insert-register' for the
meaning of prefix ARG.

(fn REG &optional ARG)" t nil) (autoload 'consult-register-store "consult-register" "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally deleting
the region when a prefix ARG is given. With a numeric prefix ARG, store/add the
number. Otherwise store point, frameset, window or kmacro.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-register" '("consult-register-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-selectrum" '("consult-selectrum--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-vertico" '("consult-vertico--"))) (autoload 'consult-xref "consult-xref" "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

(fn FETCHER &optional ALIST)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-xref" '("consult-xref--"))) (provide 'consult-autoloads)) "embark-consult" ((embark-consult-autoloads embark-consult) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "embark-consult" '("embark-consult-"))) (provide 'embark-consult-autoloads)) "avy" ((avy-autoloads avy) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil) (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t nil) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t nil) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t nil) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t nil) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t nil) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t nil) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t nil) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t nil) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don\342\200\231t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t nil) (autoload 'avy-setup-default "avy" "Setup the default shortcuts." nil nil) (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t nil) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-"))) (provide 'avy-autoloads)) "ace-window" ((ace-window-autoloads ace-window) (autoload 'ace-select-window "ace-window" "Ace select window." t nil) (autoload 'ace-delete-window "ace-window" "Ace delete window." t nil) (autoload 'ace-swap-window "ace-window" "Ace swap window." t nil) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t nil) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)" nil nil) (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t nil) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-"))) (provide 'ace-window-autoloads)) "0x0" ((0x0 0x0-autoloads) (autoload '0x0-upload-file "0x0" "Choose FILE and upload it to SERVER.

(fn SERVER FILE)" t nil) (autoload '0x0-upload-text "0x0" "Upload full or region text from the current buffer to SERVER.

(fn SERVER)" t nil) (autoload '0x0-upload-kill-ring "0x0" "Upload content from the \"kill-ring\" to SERVER.

(fn SERVER)" t nil) (autoload '0x0-shorten-uri "0x0" "Shorten the given URI with SERVER.

(fn SERVER URI)" t nil) (autoload '0x0-popup "0x0" "Create a new buffer, fill it with content, and upload it to SERVER later.

(fn SERVER)" t nil) (autoload '0x0-dwim "0x0" "Try to guess what to upload to SERVER.

(fn SERVER)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "0x0" '("0x0-"))) (provide '0x0-autoloads)) "zig-mode" ((zig-mode-autoloads zig-mode) (autoload 'zig-toggle-format-on-save "zig-mode" "Switch format before save on current buffer." t nil) (autoload 'zig-compile "zig-mode" "Compile using `zig build`." t nil) (autoload 'zig-build-exe "zig-mode" "Create executable from source or object file." t nil) (autoload 'zig-build-lib "zig-mode" "Create library from source or assembly." t nil) (autoload 'zig-build-obj "zig-mode" "Create object from source or assembly." t nil) (autoload 'zig-test-buffer "zig-mode" "Test buffer using `zig test`." t nil) (autoload 'zig-run "zig-mode" "Create an executable from the current buffer and run it immediately." t nil) (autoload 'zig-format-buffer "zig-mode" "Format the current buffer using the zig fmt." t nil) (autoload 'zig-mode "zig-mode" "A major mode for the Zig programming language.

\\{zig-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "zig-mode" '("colorize-compilation-buffer" "zig-"))) (provide 'zig-mode-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 11 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "embark" nil "embark-consult" nil "consult" nil "ace-window" nil "avy" nil "cl-lib" nil "0x0" nil "zig-mode" nil)) melpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "embark" (embark :type git :flavor melpa :files ("embark.el" "embark.texi" "embark-pkg.el") :host github :repo "oantolin/embark") "embark-consult" (embark-consult :type git :flavor melpa :files ("embark-consult.el" "embark-consult-pkg.el") :host github :repo "oantolin/embark") "consult" (consult :type git :flavor melpa :host github :repo "minad/consult") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "cl-lib" nil "0x0" (0x0 :type git :flavor melpa :host gitlab :repo "willvaughn/emacs-0x0") "zig-mode" (zig-mode :type git :flavor melpa :host github :repo "ziglang/zig-mode"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "embark" "embark-consult" "consult" "ace-window" "avy" "cl-lib" "0x0" "zig-mode")

t
