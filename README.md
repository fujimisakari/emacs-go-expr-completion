# emacs-go-expr-completion

[![MELPA](https://melpa.org/packages/go-expr-completion-badge.svg)](https://melpa.org/#/go-expr-completion)

This package is based on a [vim-go-expr-completion](https://github.com/110y/vim-go-expr-completion), to complete a left-hand side from given expression for Go.

![emacs-go-expr-completion](https://user-images.githubusercontent.com/1451667/89499453-017db480-d7fb-11ea-95d6-bf132a99d6ac.gif)

## Installation

- Install [go-expr-completion](https://github.com/110y/go-expr-completion) first.
- This package is available on [MELPA](https://melpa.org/)<br />
You can install package with the following command.

<kbd>M-x package-install [RET] go-expr-completion [RET]</kbd>

## Sample Configuration

add these lines to your init.el or .emacs file

```lisp
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-c") 'go-expr-completion))
```

## Usage

Navigate your cursor to the arbitrary expression, type `C-c C-c` or `M-x go-expr-completion`, and then this package completes the left-hand side for given expression (and `if err...` if necessary).
