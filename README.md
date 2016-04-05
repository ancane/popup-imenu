# Popup-imenu

[![MELPA](http://melpa.org/packages/popup-imenu-badge.svg)](http://melpa.org/#/popup-imenu)
[![MELPA Stable](http://stable.melpa.org/packages/popup-imenu-badge.svg)](http://stable.melpa.org/#/popup-imenu)

Show imenu index alist in a popup window.

![popup-imenu gif](https://github.com/ancane/popup-imenu/raw/master/popup-imenu.gif)

## Run
`M-x popup-imenu`

## Customize

### popup-imenu-position

Control popup horizontal positioning with:

* `'center` - open popup at window center
* `'fill-column` - center relative to fill-column (default setting)
* `'point` - open popup at point

```lisp
(setq popup-imenu-position 'point)
```
### popup-imenu-style

* `'flat` - flatten hierarchical imenu
* `'indent` - use whitespace indentation to show hierarchical imenu

```lisp
(setq popup-imenu-style 'indent)
```

### popup-imenu-fuzzy-match
Flx matching is enabled by default.
Disable it with:

```lisp
(setq popup-imenu-fuzzy-match nil)
```

## Keymap

```lisp
(global-set-key (kbd "C-e") 'popup-imenu)

;; Close the popup with same key
(define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
```

## Dependencies

* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/auto-complete/popup-el)
* [flx-ido](https://github.com/lewang/flx)
