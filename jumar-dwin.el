;;; jumar-dwin.el --- Preconfigured DWIN commands of jumar -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: convenience, emulations
;; URL: https://github.com/kenoss/jumar
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; Preconfigured DWIN commands of jumar.

;; Namespaces:
;;   `jumar:', `helm-jumar:' : Jumar internal.
;;   `jumar-', `helm-jumar-' : Public APIs, user commands and custom variables.
;;   `jumar-default-'        : Default functions for custom variables.
;;   `*jumar:variables*'     : Internal variables danger to touch.
;;   `jumar-dwin:'           : Jumar internal.
;;   `jumar-dwin-', `helm-jumar-dwin' : Public APIs.

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)
(require 'erfi-gauche)
(require 'erfi-emacs)
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
  :type 'bool)



;;;
;;; DWIN UI
;;;

(defvar jumar-dwin-action-control-alist
; (setq jumar-dwin-action-control-alist
  '((:current-set . nil)
    (:add-set . both)
    (:jump-set . tree)
    (:helm-set . list+tree)
    (:helm-preselect . tree)
    (:tree-add-function . (lambda (tree content _)
                            (jumar:tree-enhance-downward!/descend tree content)))
    (:list-add-function . (lambda (tree content flag)
                            (jumar:tree-enhance-last! tree content flag)))))


(defun jumar-dwin:get-jm-set (sym prefix)
  "Return appropriate set of jumarkers.
SYM := 'tree | 'list | 'current
If PREFIX is non-nil, invert SYM ('tree <-> 'list).

'tree or 'list:
  Return the value of `*jumar:jm-tree*' or `*jumar:jm-list*'.
'current:
  Refer :current-set of `jumar-dwin-action-control-alist' and call this function
  once again."
  (erfi:ecase sym
    ((tree)    (if (not prefix) *jumar:jm-tree* *jumar:jm-list*))
    ((list)    (if (not prefix) *jumar:jm-list* *jumar:jm-tree*))
    ((current) (erfi:$ jumar-dwin:get-jm-set <> prefix $
                       (or (cdr (assq :current-set jumar-dwin-action-control-alist))
                           (progn
                             (lwarn 'jumar :error "dwin :current-set is unavailable.")
                             (error "dwin :current-set is unavailable.")))))))

(defun jumar-dwin:get-add-function (sym prefix)
  (erfi:ecase sym
    ((tree)    (if (not prefix)
                   (cdr (assq :tree-add-function jumar-dwin-action-control-alist))
                   (cdr (assq :list-add-function jumar-dwin-action-control-alist))))
    ((list)    (if (not prefix)
                   (cdr (assq :list-add-function jumar-dwin-action-control-alist))
                   (cdr (assq :tree-add-function jumar-dwin-action-control-alist))))
    ((current) (erfi:$ jumar-dwin:get-add-function <> prefix $
                       (or (cdr (assq :current-set jumar-dwin-action-control-alist))
                           (progn
                             (lwarn 'jumar :error "dwin :current-set is unavailable.")
                             (error "dwin :current-set is unavailable.")))))))

(defun jumar-dwin-add-marker (prefix)
  (interactive "P")
  (let1 sym (cdr (assq :add-set jumar-dwin-action-control-alist))
    (erfi:ecase sym
      ((tree list current)
       (funcall (jumar-dwin:get-add-function sym prefix)
                (jumar-dwin:get-jm-set sym prefix) (jumar:make-jumarker) t))
      ((both)
       ;; TODO: ref-count
       (funcall (jumar-dwin:get-add-function 'tree nil) *jumar:jm-tree* (jumar:make-jumarker) t)
       (funcall (jumar-dwin:get-add-function 'list nil) *jumar:jm-list* (jumar:make-jumarker) t)))))

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

(defun jumar-dwin:jump:aux (set n)
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

(defun jumar-dwin-jump-current (prefix)
  (interactive "P")
  (let1 set (jumar-dwin:get-jm-set (cdr (assq :jump-set jumar-dwin-action-control-alist)) prefix)
    (jumar-dwin:jump:aux set 0)
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-dwin-jump-forward (prefix)
  (interactive "P")
  (let1 set (jumar-dwin:get-jm-set (cdr (assq :jump-set jumar-dwin-action-control-alist)) prefix)
    (jumar-dwin:jump:aux set +1)
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-dwin-jump-backward (prefix)
  (interactive "P")
  (let1 set (jumar-dwin:get-jm-set (cdr (assq :jump-set jumar-dwin-action-control-alist)) prefix)
    (jumar-dwin:jump:aux set -1)
    (run-hooks 'jumar-post-jump-hook)))



;;;
;;; DWIN helm UI
;;;

(defvar *helm-jumar-dwin-update-enabled* nil)
(defvar *helm-jumar-dwin-update-preselect-node* nil)

(defadvice helm-update (after helm-jumar-dwin-update activate)
  "Enable preselect with REAL at the beginning of `helm-jumar-dwin-jumarkers'."
  (when *helm-jumar-dwin-update-enabled*
    (helm-jumar:preselect-with-real *helm-jumar-dwin-update-preselect-node* 'eq t)
    (setq *helm-jumar-dwin-update-enabled* nil)))

(defun helm-jumar-dwin-jumarkers (prefix)
  ""
  (interactive "P")
  (dynamic-let
      ((*helm-jumar-dwin-update-enabled* t)
       (*helm-jumar-dwin-update-preselect-node*
        (jumar:tree-current (jumar-dwin:get-jm-set (cdr (assq :helm-preselect
                                                              jumar-dwin-action-control-alist))
                                                   prefix))))
    (let1 sources (erfi:ecase (cdr (assq :helm-set jumar-dwin-action-control-alist))
                    ((list)      '(helm-source-jumarkers-list))
                    ((tree)      '(helm-source-jumarkers-tree))
                    ((list+tree) '(helm-source-jumarkers-list helm-source-jumarkers-tree))
                    ((tree+list) '(helm-source-jumarkers-tree helm-source-jumarkers-list)))
      (helm-jumar:with-temporary-patch
       (helm :sources sources
             :buffer "*helm jumar*"
             :keymap helm-jumar-map
             :truncate-lines t)))))



;;;
;;; Preconfigured schemes
;;;

(defun jumar-dwin-use-preconfigured-scheme (sym)
  "Set `jumar-dwin-action-control-alist' to the preconfigured one.

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
       (setq jumar-dwin-action-control-alist
             `((:current-set . nil)
               (:add-set . both)
               (:jump-set . list)
               (:helm-set . list+tree)
               (:helm-preselect . list)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((tree-main)
       (setq jumar-dwin-action-control-alist
             `((:current-set . nil)
               (:add-set . both)
               (:jump-set . tree)
               (:helm-set . tree+list)
               (:helm-preselect . tree)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((list-only)
       (setq jumar-dwin-action-control-alist
             `((:current-set . nil)
               (:add-set . list)
               (:jump-set . list)
               (:helm-set . list)
               (:helm-preselect . list)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((tree-only)
       (setq jumar-dwin-action-control-alist
             `((:current-set . nil)
               (:add-set . tree)
               (:jump-set . tree)
               (:helm-set . tree)
               (:helm-preselect . tree)
               (:tree-add-function . ,tree-fn)
               (:list-add-function . ,list-fn*))))
      ((list+history)
       (setq jumar-dwin-action-control-alist
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

(jumar-dwin-use-preconfigured-scheme 'list+history)


(provide 'jumar-dwin)
;;; jumar-dwin.el ends here
