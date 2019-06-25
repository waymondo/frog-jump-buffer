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

``` emacs-lisp
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

The numbers `1` through `4` will cycle through the default buffer filters:

- `1`: show all buffers in the `(buffer-list)`
- `2`: filter buffers to the same mode as `current-buffer`
- `3`: filter buffers to only buffers represented by files
- `4`: filter buffers to the same project as `current-buffer` (requires
  [`projectiile`](https://github.com/bbatsov/projectile) to be installed)

---

If you would like to call a function that uses a specific filter
function by default, you can do so by defining a function like this:

``` emacs-lisp
(defun frog-jump-buffer-same-project ()
  (interactive)
  (let ((frog-jump-buffer-current-filter-function #'frog-jump-buffer-filter-same-project))
    (frog-jump-buffer)))
```

### Variables

#### `frog-jump-buffer-ignore-buffers`
This is a list of regexps of buffer names to ignore or buffer-matching
filter functions to use.

#### `frog-jump-buffer-max-buffers`
This is the maximum number of buffers to show in the `frog-menu`.  The
default is 12.

#### `frog-jump-buffer-default-filter`
This is the default filter to use when invoking
`frog-jump-buffer`. Shows all buffers by default.

#### `frog-jump-buffer-include-current-buffer`
Set to `nil` to remove the current buffer from always being the first option.

#### `frog-jump-buffer-posframe-parameters`
Explicit frame parameters to be used by the posframe `frog-jump-buffer` creates.

#### `frog-jump-buffer-posframe-handler`
The posframe handler that `frog-jump-buffer' should use. Defaults to the point being the bottom left
point of the posframe.

#### `frog-jump-buffer-filter-actions`
These are the built-in buffer filter actions available during
`frog-jump-buffer`.  Each action is a list of the form `(KEY
DESCRIPTION FILTER-FUNCTION)`. You can add additional buffer filters
like so:

``` emacs-lisp
(defun frog-jump-buffer-filter-special-buffers (buffer)
  (with-current-buffer buffer
    (-any? #'derived-mode-p '(comint-mode magit-mode inf-ruby-mode rg-mode compilation-mode))))

(add-to-list
 'frog-jump-buffer-filter-actions
 '("5" "[special]" frog-jump-buffer-filter-special-buffers) t)
```
Each filter function receives a buffer as an argument and should
return a non-nil value if that buffer should be displayed in `frog-menu-buffer`.

The built-in filter functions, bound to `1`, `2`, `3`, `4` are:

*  `frog-jump-buffer-filter-all`
*  `frog-jump-buffer-filter-same-project`
*  `frog-jump-buffer-filter-same-mode`
*  `frog-jump-buffer-filter-file-buffers`

See `frog-menu-buffer.el` for more details.
