# Popup-imenu

Select imenu items via popup window with fuzzy matching.

## popup-imenu
Opens the popup.

## popup-imenu-position

Variable controls popup horizontal positioning.
Possible values are:

*  'center - opens popup at window center
*  'fill-column - center relative to fill-column (default setting)
*  'point - open popup at point

```
(setq popup-imenu-position 'point)
```

## popup-imenu-fuzzy-match
Fuzzy matching is enabled by default.
Disables it with:

```
(setq popup-imenu-fuzzy-match nil)
```

## Keymap

```
(global-set-key (kbd "C-e") 'popup-imenu)

;; Close the popup with same key
(define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
```

## Dependencies

* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/auto-complete/popup-el)
* [flx-ido](https://github.com/lewang/flx)
