## Frog Jump Buffer

### Description

`frog-jump-buffer` is the fastest buffer-jumping Emacs lisp package
around. It is the spiritual successor to
[`ace-jump-buffer`](https://github.com/waymondo/ace-jump-buffer) and is
powered by [`avy`](https://github.com/abo-abo/avy) via
[`frog-menu`](https://github.com/clemera/frog-menu). Just like
`ace-jump-buffer`, it allows you to hop to any Emacs buffer in 2-3 key
strokes.

![example](example.gif)

### Installation

You can install it from [MELPA](http://melpa.org/) with `M-x package-install` or here’s a basic `use-package` declaration:

```emacs-lisp
(use-package frog-jump-buffer :ensure t)
```

### Usage

#### `(frog-jump-buffer)`

This is the main entry-point. Bind it to your preferred key-binding.

It opens the `frog-menu` buffer selector. The buffers appear in order
of most recent display or selection.

Selecting the `avy` character next to a buffer switches to that
buffer.

Use `0` to toggle between opening in the same window or
`(other-window)`.

The numbers `1` through `6` will cycle through the default buffer filters:

- `1`: show all buffers in the `(buffer-list)`
- `2`: filter buffers to the same mode as `current-buffer`
- `3`: filter buffers to only buffers represented by files
- `4`: show buffers from `recentf`, which can include closed buffers
- `5`: filter buffers to the same project as `current-buffer` (requires
  [`projectiile`](https://github.com/bbatsov/projectile) to be installed)
- `6`: filter buffers to similarly named buffers. (i.e. if `(current-buffer)` is `frog.html`, show
  `frog.css`, `*magit: frog*`, etc.)

### Variables

#### `frog-jump-buffer-ignore-buffers`

This is a list of regexps of buffer names to ignore or buffer-matching
filter functions to use. If you want to cut down on the noise of unimportant buffers, you will
want to configure this. Here’s an example:

```emacs-lisp
(dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                  "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))
```

#### `frog-jump-buffer-max-buffers`

This is the maximum number of buffers to show in the `frog-menu`. The
default is 12.

#### `frog-jump-buffer-default-filter`

This is the default filter to use when invoking `frog-jump-buffer`. Shows all buffers by default. If
you would like to see closed buffers by default, you might want to set this to
`frog-jump-buffer-filter-recentf`.

#### `frog-jump-buffer-include-current-buffer`

Set to `nil` to remove the current buffer from always being the first option.

#### `frog-jump-buffer-posframe-parameters`

Explicit frame parameters to be used by the posframe `frog-jump-buffer` creates.

#### `frog-jump-buffer-posframe-handler`

The posframe handler that `frog-jump-buffer` should use. Defaults to `(point)` being the bottom left
point of the posframe.

#### `frog-jump-buffer-default-filters-capital-letters`

Set to a non-nil value to use capital letters instead of numbers for the default filter actions.

#### `frog-jump-buffer-use-default-filter-actions`

Set to `nil` to only use the filter actions defined in `frog-jump-buffer-filter-actions`.

#### `frog-jump-buffer-filter-actions`

This is an empty list available for adding user defined buffer filter actions to be available during
`frog-jump-buffer`.

### Custom Filter Actions

You can add your own custom filter actions. Each action is a list of the form `(KEY DESCRIPTION FILTER-FUNCTION)`. Each filter function receives a buffer as an argument and should return a non-nil
value if that buffer should be considered display-worthy in `frog-menu-buffer`.

Example usage:

```emacs-lisp
(defun frog-jump-buffer-filter-special-buffers (buffer)
  (with-current-buffer buffer
    (-any? #'derived-mode-p '(comint-mode magit-mode inf-ruby-mode rg-mode compilation-mode))))

(setq frog-jump-buffer-filter-actions
 '("7" "[special]" frog-jump-buffer-filter-special-buffers))
```

If you would like to call a function that uses a specific filter
function by default, you can do so by defining a function like this:

```emacs-lisp
(defun frog-jump-buffer-same-project ()
  (interactive)
  (let ((frog-jump-buffer-current-filter-function #'frog-jump-buffer-filter-same-project))
    (frog-jump-buffer)))
```

See `frog-menu-buffer.el` for more details.
