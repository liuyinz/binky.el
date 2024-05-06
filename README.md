# binky

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/binky-badge.svg)](http://melpa.org/#/binky)

Jump between points like a rabbit !

This package provides commands to jump between points in buffers and files.
Pin position, float position and last jump position are all supported in
same mechanism like `point-to-register' and `register-to-point' but with an
enhanced experience.

<!-- markdown-toc start -->

## Contents

- [binky](#binky)
  - [Screenshot](#screenshot)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [Feature](#feature)
  - [Comparison](#comparison)
  - [Todo](#todo)
  - [FAQ](#faq)
  - [Donate](#donate)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x binky-*` related command

![binky with preview](image/binky-preview.jpg)

`?` for toggle preview manually, `C-g` or `escape` for quit, `SPC` to toggle in groups

- Call `M-x binky-margin-mode`

![margin with sign](image/margin-with-sign.png)

- Or use mark as indicator, `(setq binky-margin-string nil)`

![margin with mark](image/margin-with-mark.png)

## Install

### dependencies

- Emacs, version >= 26.3

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA].
Install with `M-x package-install` `RET` `binky` within Emacs.

## Usage

```elisp
;; Directly
(require 'binky)
(binky-mode)
;; margin highlight support
(require 'binky-margin)
(binky-margin-mode)

;; Or with `use-package`, etc
(use-package binky
  :hook (after-init-hook . (lambda () (binky-mode) (binky-margin-mode))))
```

- `binky-add` : add current position to records

- `binky-delete` : delete existed position from records

- `binky-jump` : jump to position of records

- `binky-jump-other-window` : jump to position of records in other window

- `binky-view` : view the position of records in other window without jumping

- `binky-binky` : one command to rule all

  - press `j`, if record `j` exists, call `binky-jump`, otherwise call `binky-add` to add pin record.
  - press `J`, if record `J` exists, call `binky-jump`, otherwise call `binky-add` to add float record.
  - press `ctrl-j`, call `binky-delete` to delete `j` record.
  - press `alt-j`, call `binky-view` to see record `j` in other window.

  With `C-u` prefix, keep editing until quit

- `binky-margin-mode` : toggle to enable or disable margin indicator feature

- `binky-next-in-buffer`/`binky-previous-in-buffer`: jump between in current buffer

- `binky-save`/`binky-restore`: save and restore pin records from file

## Customization

Lots of options are provided, see customization part in file.

## Feature

- Better UI experience than `register-to-point`, such as preview customization, jump highlight and margin indicator
- Integration with buffers switch to record last point position as float position
- Vim-like mark and jump style.

## Comparison

[Dogears](https://github.com/alphapapa/dogears.el)

- Bookmark style record
- More information: relevance, within, directory, etc

[Binky](https://github.com/liuyinz/binky.el)

- Register style record
- Vim-like mark and jump
- Flexible and customizable

I have used dogears for several months, and it's great in most features except lack of [Vim-marks](https://vim.fandom.com/wiki/Using_marks) style jumping feature
which is top priority for me. While evil is too heavy, so I wrote this package to imitate parts of features.

Enjoy it.

## Todo

- [x] Avoid adding duplicated records
- [x] Margin indicator support
- [x] ~~More sorting strategy: frecency, duration~~
- [x] Add `SPC` to toggle preview records in groups
- [x] Show same buffer records first in groups view
- [x] Add command to store records cross sessions
- [ ] optimize preview color and style for easily lookup
- [ ] support for xref history
- [ ] Fringe indicator support

## FAQ

- `binky-margin` conflicts with other packages like flymake, [flycheck](https://github.com/flycheck/flycheck), [diff-hl](https://github.com/dgutov/diff-hl), linum-mode(built-in), etc ?

  `binky` supports only margin for now, so solutions here :

  - Use fringe in other packages

  ```elisp
  ;; flycheck
  ;; Use left-fringe or right-fringe
   (setq flycheck-indication-mode 'left-fringe)

  ;; diff-hl
  ;; use fringe by default, don't turn on diff-hl-margin-mode

  ;; linum-mode
  ;; don't support fringe, use display-line-numbers-mode (since 26.0.50) instead
  ```

  - Use different side of margin in other packages

  ```elisp
  (setq binky-indicator-side 'left)

  ;; flycheck, different margin side
  (setq flycheck-indication-mode 'right-margin)

  ;; diff-hl, different margin side
  (setq diff-hl-side 'right)
  (diff-hl-margin-mode)
  ```

- All records position change to 1 after run `format-all-buffer` command ?

  [format-all](https://github.com/lassik/emacs-format-all-the-code) formats buffer by overwriting it's entire content, all markers position would be change to position 1. Please use other formatters like [apheleia](https://github.com/radian-software/apheleia) instead.

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<a href="https://paypal.me/liuyinz" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>

[melpa]: http://melpa.org/#/binky
