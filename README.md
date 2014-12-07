Jumar
=====

Emacs extension: Jump and marker like in Vim

Jumar, standing for jump and marker like in Vim, is an extension of Emacs which
gives more rich "marker" environment.


### Short video

http://recordit.co/jLoUv7oeW0


## Requirement

- Emacs 24 or later
- ERFI (https://github.com/kenoss/erfi)
- Helm (If one use visualizer.  Recent version is prefered.)


## Instration

Jumar is not available on MELPA but we'll apply to add.

Download ERFI and jumar.  Add to load-path:

```emacs-lisp
(add-to-list 'load-path "/path/to/erfi/lisp")
(add-to-list 'load-path "/path/to/jumar")
```


## Simple usage

Add the following code to your `.emacs` file:

```emacs-lisp
(require 'jumar)
(require 'jumar-dwin)
(require 'helm)  ; if you use visualizer.

;; If one needs highlight the line after jump.
(add-hook 'jumar-post-jump-hook 'jumar-misc-hl-turn-on-until-next-command)

(jumar-dwin-use-preconfigured-scheme 'list+history)
(jumar-init)

;; As you like.
(define-key global-map (kbd "C-'")     'jumar-dwin-add-marker)
(define-key global-map (kbd "C-\"")    'jumar-dwin-jump-current)
(define-key global-map (kbd "C-,")     'jumar-dwin-jump-backward)
(define-key global-map (kbd "C-.")     'jumar-dwin-jump-forward)
(define-key global-map (kbd "C-x C-'") 'helm-jumar-dwin-jumarkers)

;; As you need.  Advise jump commands, like `find-tag' and `gtags-find-tag',
;; to add jumarker before/after jump.
(jumar-dwin-advise-jump-command-to-add-jumarker 'find-tag)
; (jumar-dwin-advise-jump-command-to-add-jumarker 'elisp-slime-nav-find-elisp-thing-at-point)
```

For the first time, reload `.emacs` (or restart Emacs) and evaluate the following:

```
(save-excursion
  (goto-char (window-start))
  (dotimes (i 10)
    (call-interactively 'jumar-dwin-add-marker)
    (forward-line 2)))
```

Then, invoking `C-x C-'` (`helm-jumar-dwin-jumarkers`) shows markers with helm interface.
You can jump to the selected marker.  You can peep marker with `C-z`
(`helm-execute-persistent-action`).

You can jump without helm.  `C-.` (`jumar-dwin-jump-forward`) is jump forward and
`C-,` (`jumar-dwin-jump-backward`) is backward.  `C-'` (`jumar-dwin-add-marker`) add new marker.

Try to add new marker in other buffer, kill that buffer and invoke `C-x C-'`.  You can see
"Killed buffer" at the last of list of markers.  Jumar automatically manage "unavailable markers".
Jumping to that one try to reopen file and revive marker positions in that buffer.
By default, `jumar-dwin-jump-forward` and `jumar-dwin-jump-backward` skip unavailable markers
except for the case that is the last/first one.


### Command overview (with above setting)

| Key       | Command                     | Description                                 |
| --------- | --------------------------- | ------------------------------------------- |
| `C-'`     | `jumar-dwin-add-marker`     | Add marker                                  |
| `C-"`     | `jumar-dwin-jump-current`   | Jump to the current marker                  |
| `C-,`     | `jumar-dwin-jump-backward`  | Jump to previous marker                     |
| `C-.`     | `jumar-dwin-jump-forward`   | Jump to next marker                         |
| `C-x C-'` | `helm-jumar-dwin-jumarkers` | View markers with helm (and execute action) |

### Commands in Helm session

| Key       / Guide on `C-i` (`helm-select-action`) | Command                          | Description                                        |
| ------------------------------------------------- | -------------------------------- | -------------------------------------------------- |
| `Enter`   / Jump to marker                        | `helm-jumar-jump/set-current`    | Set selected marker to the current and jump        |
| `C-z`                                             | `helm-jumar-persistent-action`   | Peep marker on the current line                    |
| `C-u C-z`                                         | `helm-jumar-persistent-action`   | Delete marker on the current line                  |
| `C-c d`   / Delete markers below                  | `helm-jumar-delete-nodes-below`  | Delete markers under the current line              |
| `C-c k`   / Delete marker(s)                      | `helm-jumar-delete-marked-nodes` | Delete marked markers                              |
| `M-f`     / Forward branch                        | `helm-jumar-forward-branch`      | Change branch forward                              |
| `M-b`     / Backward branch                       | `helm-jumar-backward-branch`     | Change branch backward                             |
| `C-r`                                             | `helm-jumar-run-recenter`        | Redisplay candidates with selected marker centered |
