;;; jumar-dwim.el --- Preconfigured DWIM commands of jumar

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: tools, convenience, emulations
;; URL: https://github.com/kenoss/jumar
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; Preconfigured DWIM commands of jumar.

;; Namespaces:
;;   `jumar:', `helm-jumar:' : Jumar internal.
;;   `jumar-', `helm-jumar-' : Public APIs, user commands and custom variables.
;;   `jumar-default-'        : Default functions for custom variables.
;;   `jumar:*variables*'     : Internal variables danger to touch.
;;   `jumar-dwim:'           : Jumar internal.
;;   `jumar-dwim-', `helm-jumar-dwim' : Public APIs.

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)
(require 'erfi-gauche)
(require 'erfi-misc)

(require 'jumar)



;;;
;;; Custom variables
;;;

(defcustom jumar-skip-unavailable-jumarker t
  "If non-nil, functions like `jumar-jump-forward' skip jumarkers
whose state is not 'available.  Otherwise try to reopen with
`jumar-reopen-function'."
  :group 'jumar
  :type 'boolean)



;;;
;;; DWIM UI
;;;

(defvar jumar-dwim-action-control-alist
  '((:current-set . nil)
    (:add-set . both)
    (:jump-set . tree)
    (:helm-set . list+tree)
    (:helm-preselect . tree)
    (:tree-add-function . (lambda (tree content _)
                            (jumar:tree-enhance-downward!/descend tree content)))
    (:list-add-function . (lambda (tree content flag)
                            (jumar:tree-enhance-last! tree content flag)))))


(defun jumar-dwim:get-jm-set (sym prefix)
  "Return appropriate set of jumarkers.
SYM := 'tree | 'list | 'current
If PREFIX is non-nil, invert SYM ('tree <-> 'list).

'tree or 'list:
  Return the value of `jumar:*jm-tree*' or `jumar:*jm-list*'.
'current:
  Refer :current-set of `jumar-dwim-action-control-alist' and call this function
  once again."
  (erfi:ecase sym
    ((tree)    (if (not prefix) jumar:*jm-tree* jumar:*jm-list*))
    ((list)    (if (not prefix) jumar:*jm-list* jumar:*jm-tree*))
    ((current) (jumar-dwim:get-jm-set
                (or (cdr (assq :current-set jumar-dwim-action-control-alist))
                    (progn
                      (lwarn 'jumar :error "dwim :current-set is unavailable.")
                      (error "dwim :current-set is unavailable.")))
                prefix))))

(defun jumar-dwim:get-add-function (sym prefix)
  (erfi:ecase sym
    ((tree)    (if (not prefix)
                   (cdr (assq :tree-add-function jumar-dwim-action-control-alist))
                   (cdr (assq :list-add-function jumar-dwim-action-control-alist))))
    ((list)    (if (not prefix)
                   (cdr (assq :list-add-function jumar-dwim-action-control-alist))
                   (cdr (assq :tree-add-function jumar-dwim-action-control-alist))))
    ((current) (jumar-dwim:get-add-function
                (or (cdr (assq :current-set jumar-dwim-action-control-alist))
                    (progn
                      (lwarn 'jumar :error "dwim :current-set is unavailable.")
                      (error "dwim :current-set is unavailable.")))
                prefix))))

(defun jumar-dwim-add-marker (&optional prefix)
  (interactive "P")
  (let1 sym (cdr (assq :add-set jumar-dwim-action-control-alist))
    (erfi:ecase sym
      ((tree list current)
       (funcall (jumar-dwim:get-add-function sym prefix)
                (jumar-dwim:get-jm-set sym prefix) (jumar:make-jumarker) t))
      ((both)
       ;; TODO: ref-count
       (funcall (jumar-dwim:get-add-function 'tree nil) jumar:*jm-tree* (jumar:make-jumarker) t)
       (funcall (jumar-dwim:get-add-function 'list nil) jumar:*jm-list* (jumar:make-jumarker) t)))
    (jumar:message "Add marker.")))

(defun jumar:node-nth-child (node n)
  "Return list (next-node rest).  Here,
  rest is an integer.
  next-node is a node that is next (N - rest)-th child if N is positive.
  -(N - rest)-th parent if N is negative.  If rest is non-zero, next-node
  is leaf or root.
If N is zero, return (NODE 0).

N must be an integer."
  (cond ((< n 0)
         (erfi:let lp ((node node) (n n))
           (if (or (zerop n) (jumar:node-root? node))
               `(,node ,n)
               (lp (jumar:node-parent node) (+ n 1)))))
        ((zerop n)
         `(,node 0))
        (t
         (erfi:let lp ((node node) (n n))
           (if (or (zerop n) (jumar:node-leaf? node))
               `(,node ,n)
               (lp (jumar:node-child node) (- n 1)))))))

(defun jumar-dwim:jump:aux (set n)
  "Try to jump N-th next element of SET.

N must be an integer.

Side effect: if jump success, set current node of SET to that one."
  (cond ((jumar:tree-empty? set)
         (jumar:message "There is no marker."))
        ((zerop n)
         (jumar:jumarker-goto (jumar:node-content (jumar:tree-current set))))
        (t
         (let1 direction (/ n (abs n))
           (erfi:let lp ((node (jumar:tree-current set)) (n n) (msg ""))
             (destructuring-bind (next r) (jumar:node-nth-child node n)
               (cond ((not (zerop r))
                      (if jumar-jump-current-if-no-further-marker
                          (progn
                            (setf (jumar:tree-current set) next)
                            (jumar:jumarker-goto (jumar:node-content next))
                            (jumar:message (concat msg "There is no further marker, jump %s one.")
                                           (if (= direction 1) "last" "first")))
                          (jumar:message (concat msg "There is no further marker."))))
                     ((and jumar-skip-unavailable-jumarker
                           (not (eq 'available (jumar:jumarker-state (jumar:node-content next)))))
                      (lp next direction "Some jumarkers skipped.  "))
                     (t
                      (setf (jumar:tree-current set) next)
                      (jumar:jumarker-goto (jumar:node-content next))
                      (jumar:message (concat msg "Jumped!"))))))))))

(defun jumar-dwim-jump-current (&optional prefix)
  (interactive "P")
  (let1 set (jumar-dwim:get-jm-set (cdr (assq :jump-set jumar-dwim-action-control-alist)) prefix)
    (jumar-dwim:jump:aux set 0)
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-dwim-jump-forward (&optional prefix)
  (interactive "P")
  (let1 set (jumar-dwim:get-jm-set (cdr (assq :jump-set jumar-dwim-action-control-alist)) prefix)
    (jumar-dwim:jump:aux set +1)
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-dwim-jump-backward (&optional prefix)
  (interactive "P")
  (let1 set (jumar-dwim:get-jm-set (cdr (assq :jump-set jumar-dwim-action-control-alist)) prefix)
    (jumar-dwim:jump:aux set -1)
    (run-hooks 'jumar-post-jump-hook)))



;;;
;;; DWIM Helm UI
;;;

(defvar *helm-jumar-dwim-update-enabled* nil)
(defvar *helm-jumar-dwim-update-preselect-node* nil)

(defadvice helm-update (after helm-jumar-dwim-update activate)
  "Enable preselect with REAL at the beginning of `helm-jumar-dwim-jumarkers'."
  (when (and *helm-jumar-dwim-update-enabled* *helm-jumar-dwim-update-preselect-node*)
    (helm-jumar:preselect-with-real *helm-jumar-dwim-update-preselect-node* 'eq t)
    (setq *helm-jumar-dwim-update-enabled* nil)))

(defun helm-jumar-dwim-jumarkers (&optional prefix)
  "DWIM version preconfigured `helm' for jump to jumarkers."
  (interactive "P")
  (dynamic-let
      ((*helm-jumar-dwim-update-enabled* t)
       (*helm-jumar-dwim-update-preselect-node*
        (jumar:tree-current (jumar-dwim:get-jm-set (cdr (assq :helm-preselect
                                                              jumar-dwim-action-control-alist))
                                                   prefix))))
    (let1 sources (erfi:ecase (cdr (assq :helm-set jumar-dwim-action-control-alist))
                    ((list)      '(helm-source-jumarkers-list))
                    ((tree)      '(helm-source-jumarkers-tree))
                    ((list+tree) '(helm-source-jumarkers-list helm-source-jumarkers-tree))
                    ((tree+list) '(helm-source-jumarkers-tree helm-source-jumarkers-list)))
      (helm :sources sources
            :buffer "*helm jumar*"
            :keymap helm-jumar-map
            :truncate-lines t))))


;;;
;;; Advice for commands like tag jump
;;;

(defun jumar-dwim-advise-jump-command-to-add-jumarker (command-symbol)
  "Advise \"jump\" command COMMAND-SYMBOL to add jumarker before/after jump.

Before jump, add jumarker at the point unless the current jumarker is
on the same line.
After jump, add jumarker."
  (eval `(defadvice ,command-symbol (around jumar activate)
          "Add jumarker to go back original point.

Before jump, add jumarker at the point unless the current jumarker is
on the same line.
After jump, add jumarker."
          (progn
            (let* ((sym (cdr (assq :add-set jumar-dwim-action-control-alist)))
                   (sym (if (eq 'both sym)
                            (cdr (assq :jump-set jumar-dwim-action-control-alist))
                            sym))
                   (jm-set (jumar-dwim:get-jm-set sym nil))
                   (current (jumar:tree-current jm-set))
                   (jm (and current (jumar:node-content current))))
              (if (not (and current (eq 'available (jumar:jumarker-state jm))))
                  (jumar-dwim-add-marker)
                  (let ((m (jumar:jumarker-marker jm)))
                    (unless (and (eq (current-buffer) (marker-buffer m))
                                 (= (line-number-at-pos) (line-number-at-pos (marker-position m))))
                      (jumar-dwim-add-marker)))))
            ad-do-it
            (jumar-dwim-add-marker)))
        t))



;;;
;;; Preconfigured schemes
;;;

(defun jumar-dwim-use-preconfigured-scheme (sym)
  "Set `jumar-dwim-action-control-alist' to the preconfigured one.

SYM can be one of the following symbols:

  'list-main :
    Mainly use list for set of jumarkers.  Tree is auxiliary.

  'tree-main :
    Like 'list-main, but mainly use tree.

  'list-only :
    Only use list for set of jumarkers.

  'tree-only :
    Like 'list-only, but only use tree.

  'list+history :
    Like 'list-main, but use tree as list containing all jumarkers."
  (let ((tree-fn  (lambda (tree content _)
                    (jumar:tree-enhance-downward!/descend tree content)))
        (list-fn  (lambda (tree content update-current-flag)
                    (jumar:tree-enhance-last! tree content update-current-flag)))
        (list-fn* (lambda (tree content update-current-flag)
                    (jumar:tree-add!/child-replaced tree content nil update-current-flag))))
    (erfi:case sym
      ((list-main)
       (setq jumar-dwim-action-control-alist
             `((:current-set . nil)
               (:add-set . both)
               (:jump-set . list)
               (:helm-set . list+tree)
               (:helm-preselect . list)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((tree-main)
       (setq jumar-dwim-action-control-alist
             `((:current-set . nil)
               (:add-set . both)
               (:jump-set . tree)
               (:helm-set . tree+list)
               (:helm-preselect . tree)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((list-only)
       (setq jumar-dwim-action-control-alist
             `((:current-set . nil)
               (:add-set . list)
               (:jump-set . list)
               (:helm-set . list)
               (:helm-preselect . list)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((tree-only)
       (setq jumar-dwim-action-control-alist
             `((:current-set . nil)
               (:add-set . tree)
               (:jump-set . tree)
               (:helm-set . tree)
               (:helm-preselect . tree)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((list+history)
       (setq jumar-dwim-action-control-alist
             `((:current-set . nil)
               (:add-set . both)
               (:jump-set . list)
               (:helm-set . list+tree)
               (:helm-preselect . list)
               (:tree-add-function . ,list-fn)
               (:list-add-function . ,list-fn*))))
      )))



;;;
;;; Defualt
;;;

(jumar-dwim-use-preconfigured-scheme 'list+history)


(provide 'jumar-dwim)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; jumar-dwim.el ends here
