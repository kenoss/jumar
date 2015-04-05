;;; jumar.el --- Jump and marker like in Vim, with Helm interface.

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Maintainer: Ken Okada <keno.ss57@gmail.com>
;; Keywords: tools, convenience, emulations
;; URL: https://github.com/kenoss/jumar
;; Package-Requires: ((emacs "24") (cl-lib "0.3") (erfi "0.1"))

;; Apache License, Version 2.0

;;; Commentary:

;; Jump and marker like in Vim, a tool to climb source code mountain.
;;
;; This package provides another marker enviroment.  Emacs's default mark(er) is
;; very restricted; a lot of commands modify marks without asking and there's no
;; option to control their behavior; controlling marks is only available with
;; mark-ring and only non user frendly commands are provided; also `pop-tag-mark'
;; does not allow bi-directional movement; global mark-ring discards inaccessible
;; markers and one cannot jump with reopening that file; one cannot view marks
;; while Vim provides `:jumps' command.
;;
;; A solution jumar proposes is as the following:
;;   - A new marker object `jumar:jumarker': It allows one to jump to points
;;     in other files.  Accessibility of buffers is automatically managed.
;;     When file is reopened, observer try to revive jumarkers in that file.
;;     One can jump to jumarker in killed file buffer.
;;   - By default, one can use tree and list for set of jumarkers.  Simultaneous
;;     use is allowed.  Tree is like `undo-tree'; List is like markers in Vim.
;;     By default, some use cases are provided with DWIM commands.
;;   - For default visualizer UI, Helm is used: User can look over sets of
;;     jumarkers select target and action; peep, jump and delete.  For tree,
;;     one can change branches easily.  (Use of visualiver is optional.)
;;
;; Jumar does not alter "mark" in Emacs, which is set many commands automatically.
;; Jumar is an environment to manage "marker", which users can use freely;
;; jumar only helps one to save and manage points manually.  If automatic saving
;; is necessary, one can use functions provided by jumar for hooks.
;;
;; For more details, see ./README.md .

;; Namespaces:
;;   `jumar:', `helm-jumar:' : Jumar internal.
;;   `jumar-', `helm-jumar-' : Public APIs, user commands and custom variables.
;;   `jumar-default-'        : Default functions for custom variables.
;;   `jumar:*variables*'     : Internal variables danger to touch.

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'cl-lib)                     ; for `jumar:d-let*'
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)
(require 'erfi-gauche)
(require 'erfi-misc)



;;;
;;; Custom variables
;;;

(defgroup jumar nil
  "Jump and marker like in Vim."
  :group 'emacs)

(defcustom jumar-jump-current-if-no-further-marker t
  "If non-nil, jump jumarker of current node when there is no further jumarker."
  :group 'jumar
  :type 'boolean)

(defcustom jumar-message-function 'message
  "A function used in `jumar:message' to print messages."
  :group 'jumar
  :type 'function)

(defcustom jumar-reopen-file-function 'jumar-default-reopen-file
  "A function used to reopen file.  It is called in functions like
`jumar-jump-forward', when target jumarker's state is 'unavailable.
It must return opened buffer or nil.

In this function, `jumar:jumarker-hook-find-file' must be called
to revive managed jumarkers.  (Usually it is called from `fnid-file'
with `find-file-hook'.)"
  :group 'jumar
  :type 'function)

(defcustom jumar-restore-window-start t
  "If t, `jumar:jumarker-goto' try to restore window start.
Sometimes it fails, for example, window size was changed, or jumarker was set
by emacs lisp code."
  :group 'jumar
  :type 'boolean)

(defcustom jumar-revive-marker-function 'jumar-default-revive-marker
  "A function called to revive markers in killed or reverted buffers.
It must return a newly created marker or symbol 'broken.  It is called with
two arguments: buffer, and an alist that `jumar:jumarker->saved-datum' returns.

The default value is `jumar-default-revive-marker'."
  :group 'jumar
  :type 'function)

(defcustom jumar-y-or-n-p 'y-or-n-p
  "`y-or-n-p' used in helm-jumar session.
If function, it must be like `y-or-n-p'.
If t (resp. nil), it is equivalent to returning always t (resp. nil)."
  :group 'jumar
  :type '(choice (const :tag "Always yes" t)
                 (const :tag "Always no" nil)
                 function))

(defcustom helm-jumar-candidate-number-limit '(25 24)
  "Nil or integer or list (Int Int).  Integer n is equivalent to (n n).
If (a b), maximum number of candidates of helm-jumar is a+b+1.
a (resp. b) is max of number of candidates before (resp. after) current jumarker.
Nil means no limit.

After setting this variable, one must execute `jumar-init'.
Real value is set to `*helm-jumar-candidate-number-limit*'."
  :group 'jumar)

(defcustom jumar-preview-text-length-limit 100
  "Max length of text that in previewed in helm-jumar buffer.
It must be nil or an integer.

If nil, no preview is showed.  If an integer, buffer contents of the line
at marker, form beginnig of line to the end of line or this limit, is previewed.

If one want to propertize previewed text,
use `jumar-preview-text-process-function'."
  :group 'jumar
  :type '(choice (const :tag "No preview" nil)
                 integer))

(defcustom jumar-preview-text-process-function (lambda (text pt) text)
  "A function to process preview text for helm-jumar.

This function is called with two arguments: string TEXT and integer POINT;
helm-jumar preview line text where jumarker exists.  TEXT is line content
with properties and POINT indicating place in TEXT where the marker is.
In the case that `jmuar-preview-text-length-limit' is smaller than length
of line content, length of TEXT can be than POINT, too.

This function must return a string; it is used in actual preview.

To make disable preview, use the variable `jumar-preview-text-length-limit'."
  :group 'jumar
  :type 'function)


(defvar jumar-tree-quota-default '(100 150))
;; These values are customizable, but ordinary user shouldn't touch.
;; If one use it, see "Quota" section of source code carefully.
(defvar jumar-reduce-tree-size-function 'jumar-default-reduce-tree-size)
(defvar jumar-reduce-tree-size-algorithm 'jumar-default-reduce-tree-size-algorithm)



;;;
;;; Auxiliary macro
;;;

(defmacro jumar:d-let* (bindings &rest body)
  "Successive `destructing-bind'.  Interface alikes `match-let*' in Gauche."
  (declare (indent 1))
  (cond ((null bindings)
         `(progn ,@body))
        ((and (listp bindings)
              (= 2 (length (car bindings))))
         `(cl-destructuring-bind ,@(car bindings)
            (jumar:d-let* ,(cdr bindings) ,@body)))
        (t
         (lwarn 'jumar :error "Malformed `jumar:d-let*'."))))



;;;
;;; Data structures: tree and node
;;;

(defstruct (jumar:tree
            (:constructor nil)
            (:constructor jumar:make-tree
                          (&optional
                           (content-destructor nil)
                           (quota jumar-tree-quota-default)
                           (reduce-size-function nil)
                           &aux
                           (root nil)
                           (current nil)
                           (size 0))))
  root current size content-destructor quota reduce-size-function)

;; Types: Tree a (In this package, a is `jumar:jumarker'.)
;;   root:     Maybe jumar:node a
;;   current:  Maybe jumar:node a
;;   size:     Integer
;;   content-destructor:  Maybe Function  ; destructor of contents of nodes
;;   quota:    (Integer Integer)
;;   reduce-size-function:  Maybe Function
;;
;; Rules:
;;   Trees do not share nodes.
;;   (jumar:tree-size obj) equals number of jumar:node under the ROOT of obj.
;;   (jumar:node<= ROOT CURRENT) is t.
;;   (memq (jumar:tree-current tree) (jumar:tree-breadcrumbs tree)) is non-nil.
;;   (and (<= 2 (car QUOTA)) (<= (car QUOTA) (cadr QUOTA))) is t.
;;   If non-nil, REDUCE-SIZE-FUNCITON behaves like `jumar-default-reduce-tree-size'.
;;   Note that it is not guaranteed that SIZE is less than or equal to (cadr QUOTA) .


(defstruct (jumar:node
            (:constructor nil)
            (:constructor jumar:make-node/parent
                          (content parent
                           &aux
                           (child nil)
                           (lchildren '())
                           (rchildren '())
                           (time (current-time))))
            (:constructor jumar:make-node/child
                          (content child
                           &aux
                           (parent nil)
                           (lchildren '())
                           (rchildren '())
                           (time (current-time)))))
  parent lchildren child rchildren content)

;; Types: Node a (In this package, a is `jumar:jumarker'.)
;;   parent:     Maybe jumar:node a
;;   child:      Maybe jumar:node a
;;   lchildren:  List jumar:node a
;;   rchildren:  List jumar:node a
;;   content:    a
;;
;; Rules:
;;   (eq node (jumar:node-parent (jumar:node-child node))) is t unless node is leaf.
;;   A node is root if and only if its PARENT is nil.


;;; Predicates
(defsubst jumar:tree-empty? (tree)
  (not (jumar:tree-root tree)))
(defsubst jumar:node-leaf? (node)
  (null (jumar:node-child node)))
(defsubst jumar:node-root? (node)
  (null (jumar:node-parent node)))
(defsubst jumar:tree-root? (tree node)
  (eq node (jumar:tree-root tree)))


;;; Partial order
(defun jumar:node< (n1 n2)
  "Return non-nil if and only if N1 is an ancestor of N2."
  (erfi:let lp ((node (jumar:node-parent n2)))
    (if (not node)
        nil
        (or (eq n1 node) (lp (jumar:node-parent node))))))
(defsubst jumar:node<= (n1 n2)
  "Return non-nil if and only if N1 is equal to N2 or an ancestor of N2."
  (or (eq n1 n2) (jumar:node< n1 n2)))

(defun jumar:tree-has-node? (tree node)
  (and (not (jumar:tree-empty? tree))
       (jumar:node<= (jumar:tree-root tree) node)))

;; Is it necessary?
(defun jumar:common-ancestor (n1 n2)
  "Return common ancestor if exists, nil otherwise."
  (erfi:let lp1 ((n1 n1) (n2 n2) (memory '()))
    (cond ((not n1)
           (erfi:let lp2 ((node n2) (memory memory))
             (and node (or (car-safe (member node memory))
                           (lp2 (jumar:node-parent node) (cons node memory))))))
          ((member n1 memory)           ; This is not the best but enough.
           n1)
          (t
           (lp1 n2 (jumar:node-parent n1) (cons n1 memory))))))


;;; Constructors

(defun jumar:tree-set-first-node! (tree content)
  (let1 new (jumar:make-node/parent content nil)
    (setf (jumar:tree-root tree) new)
    (setf (jumar:tree-current tree) new)
    (incf (jumar:tree-size tree))
    new))

(defun jumar:tree-enhance-upward! (tree content)
  "Make a new node that has CONTENT and renew root of TREE.  Return the new node.
This function does not run `jumar:tree-keep-size-limit!'."
  (if (jumar:tree-empty? tree)
      (jumar:tree-set-first-node! tree content)
      (let* ((root (jumar:tree-root tree))
             (new (jumar:make-node/child content root)))
        (setf (jumar:node-parent root) new)
        (setf (jumar:tree-root tree) new)
        (incf (jumar:tree-size tree))
        new)))

(defun jumar:tree-enhance-downward! (tree content &optional node update-child-flag)
  "Make a new node that has CONTENT and add to the children of NODE.
NODE defaults to current node of TREE.  Return the new node."
  (if (jumar:tree-empty? tree)
      (jumar:tree-set-first-node! tree content)
      (let* ((node (or node (jumar:tree-current tree)))
             (new (jumar:make-node/parent content node)))
        (cond ((not (jumar:node-child node))
               (setf (jumar:node-child node) new))
              (update-child-flag
               (setf (jumar:node-lchildren node)
                     (erfi:append-reverse (jumar:node-rchildren node)
                                          (cons (jumar:node-child node) (jumar:node-lchildren node))))
               (setf (jumar:node-child node) new)
               (setf (jumar:node-rchildren node) '()))
              (t
               (setf (jumar:node-rchildren node) (append (jumar:node-rchildren node) (list new)))))
        (incf (jumar:tree-size tree))
        (jumar:tree-keep-size-limit! tree)
        new)))

(defun jumar:tree-enhance-downward!/descend (tree content)
  "Make a new node that has CONTENT and add to the children of NODE.
Return the new node."
  (rlet1 new (jumar:tree-enhance-downward! tree content nil t)
    (setf (jumar:tree-current tree) new)))

(defun jumar:tree-enhance-last! (tree content &optional update-current-flag)
  "Make a new node that has CONTENT and add to the last of TREE,
regarded as list by `jumar:tree-breadcrumbs'.  Return the new node.

If UPDATE-CURRENT-FLAG is t, set the newly created node to the current node of
TREE."
  (if (jumar:tree-empty? tree)
      (jumar:tree-set-first-node! tree content)
      (erfi:let lp ((current (jumar:tree-current tree)))
        (if (not (jumar:node-leaf? current))
            (lp (jumar:node-child current))
            (rlet1 new (jumar:tree-enhance-downward! tree content current)
              (when update-current-flag
                (setf (jumar:tree-current tree) new)))))))

(defun jumar:tree-add!/child-replaced (tree content &optional node update-current-flag)
  "Make a new node that has CONTENT and replace the child of NODE with it.
Old child is deleted.
NODE defaults to current node of TREE.  Return the new node.

If UPDATE-CURRENT-FLAG is non-nil, set the new node to the current node of TREE."
  (if (jumar:tree-empty? tree)
      (jumar:tree-set-first-node! tree content)
      (let* ((node (or node (jumar:tree-current tree)))
             (new (jumar:make-node/parent content node))
             (old (jumar:node-child node)))
        (setf (jumar:node-child node) new)
        (setf (jumar:tree-size tree) (+ 1 (jumar:tree-size tree)
                                        (if old
                                            (- (jumar:node-count-nodes old))
                                            0)))
        (if update-current-flag
            (setf (jumar:tree-current tree) new)
            (when (and old (jumar:node<= old (jumar:tree-current tree)))
              (setf (jumar:tree-current tree) node)))
        (when old
          (if-let1 d (jumar:tree-content-destructor tree)
            (jumar:tree-for-each old d))))))


;;; Modifiers

(defun jumar:tree-descend-current! (tree &optional n)
;; MORE DOCUMENT
  (let1 n (or n 1)
    (cond ((< n 0)
           (jumar:tree-ascend-current! tree (- n)))
          ((= n 0)
           nil)
          (t
           (if-let1 current (jumar:tree-current tree)
             (erfi:let lp ((current current) (n (or n 1)))
               (let1 next (jumar:node-child current)
                 (cond ((not next)
                        nil)
                       ((= 1 n)
                        (setf (jumar:tree-current tree) next))
                       (t
                        (lp next (- n 1)))))))))))

(defun jumar:tree-ascend-current! (tree &optional n)
;; MORE DOCUMENT
  (let1 n (or n 1)
    (cond ((< n 0)
           (jumar:tree-descend-current! tree (- n)))
          ((= n 0)
           nil)
          (t
           (if-let1 current (jumar:tree-current tree)
             (erfi:let lp ((current current) (n n))
               (let1 next (jumar:node-parent current)
                 (cond ((not next)
                        nil)
                       ((= 1 n)
                        (setf (jumar:tree-current tree) next))
                       (t
                        (lp next (- n 1)))))))))))

(defun jumar:node-forward-branch! (node-or-tree &optional n)
  "Change chosen child of the node to the next N th child.
N defaults to 1.  If NODE-OR-TREE is a tree, use the current node.
If N is out of range, does not change anything and return nil.
If N is valid, return non-nil."
  (let ((n (or n 1))
        (node (cond ((jumar:node-p node-or-tree) node-or-tree)
                    ((jumar:tree-p node-or-tree) (jumar:tree-current node-or-tree))
                    (t (lwarn 'jumar 'erorr "Wrong argument type: node-or-tree: %s" node-or-tree)))))
    (when node
      (erfi:let lp ((n n)
                    (l (jumar:node-lchildren node))
                    (c (jumar:node-child node))
                    (r (jumar:node-rchildren node)))
        (cond ((< 0 n)
               (if (null r)
                   nil
                   (lp (- n 1) (cons c l) (car r) (cdr r))))
              ((< n 0)
               (if (null l)
                   nil
                   (lp (+ n 1) (cdr l) (car l) (cons c r))))
              (t ; i.e., (= n 0)
               (setf (jumar:node-lchildren node) l)
               (setf (jumar:node-child node) c)
               (setf (jumar:node-rchildren node) r)
               c))))))


;;; Deletion

(defun jumar:tree-delete-node! (tree &optional node delete-below-flag)
  "Delete NODE from TREE.  NODE defaults to current node of TREE.
If DELETE-BELOW-FLAG is t, delete children of NODE together.  Otherwise,
new child of parent of NODE is child of NODE.  (lchildren and rchildren of NODE
are deleted.  This means, \"delete single line in *helm jumar* buffer.\")

If new child of parent would be nil and other children exist, they will be
\"shifted\".

NODE must be in TREE.  Hence, TREE must not be empty."
  ;; Test
  (unless (jumar:tree-has-node? tree node)
    (lwarn 'jmuar :error "`jumar:tree-delete-node!': argument out of range.")
    (error "argument out of range."))
  (let1 node (or node (jumar:tree-current tree))
    (when node
      (let* ((root? (jumar:tree-root? tree node))
             (child  (if delete-below-flag nil (jumar:node-child node)))
             (parent (if root?             nil (jumar:node-parent node))))
        ;; Remove node from tree.
        (if root?
            (prog1 t
              ;; Current node will be set at last.
              (setf (jumar:tree-root tree) child)
              (when child
                (setf (jumar:node-parent child) nil)))
            ;; Link parent and child each other.  (Here parent is non-nil.)
            (prog1 t
              (when child
                (setf (jumar:node-parent child) parent))
              ;; Here we can't use `jumar:node-remove-child!' because node maybe in l(r)children.
              (cond ((eq node (jumar:node-child parent))
                     (if child
                         (setf (jumar:node-child parent) child)
                         (jumar:node-remove-child! parent)))
                    ;; For ordinary use, these two cases below will not occur.
                    ((memq node (jumar:node-lchildren parent))
                     (setf (jumar:node-lchildren parent)
                           (destructuring-bind (a b) (erfi:break (cut eq node <>)
                                                                 (jumar:node-lchildren parent))
                             (erfi:append! a (and child (list child)) (cdr b)))))
                    ((memq node (jumar:node-rchildren parent))
                     (setf (jumar:node-rchildren parent)
                           (destructuring-bind (a b) (erfi:break (cut eq node <>)
                                                                 (jumar:node-rchildren parent))
                             (erfi:append! a (and child (list child)) (cdr b)))))
                    (t (lwarn 'jumar :error "Violation of the rule")))))
        ;; Then apply destructor to the removed nodes.
        (when (not delete-below-flag)
          (setf (jumar:node-child node) nil))
        (setf (jumar:tree-size tree) (- (jumar:tree-size tree) (jumar:node-count-nodes node)))
        (if-let1 d (jumar:tree-content-destructor tree)
          (jumar:tree-for-each node d))
        ;; Set new current node if old one was deleted.
        (when (jumar:node<= node (jumar:tree-current tree))
          (setf (jumar:tree-current tree) (or child parent)))))))

(defun jumar:node-remove-child! (node-or-tree &optional prefered-direction)
  "Remove child of node and set the next one.
PREFERED-DIRECTION must be 'left or 'right, defaults to 'right.
If NODE-OR-TREE is tree, use current node.
Return t if and only if new child set.

This function does not take care of destructor."
  (let* ((pdirection (or prefered-direction 'right))
         (node (cond ((jumar:node-p node-or-tree) node-or-tree)
                     ((jumar:tree-p node-or-tree) (jumar:tree-current node-or-tree))
                     (t (lwarn 'jumar 'erorr "Wrong type of argument: node-or-tree: %s" node-or-tree))))
         (l (jumar:node-lchildren node))
         (r (jumar:node-rchildren node)))
    (if (and (null l) (null r))
        (prog1 nil
          (setf (jumar:node-child node) nil))
        (prog1 t
          (erfi:case (or prefered-direction 'right)
            ((left)  (if (not (null l))
                         (progn
                           (setf (jumar:node-lchildren node) (cdr l))
                           (setf (jumar:node-child node) (car l)))
                         (progn
                           (setf (jumar:node-rchildren node) (cdr r))
                           (setf (jumar:node-child node) (car r)))))
            ((right) (if (not (null r))
                         (progn
                           (setf (jumar:node-rchildren node) (cdr r))
                           (setf (jumar:node-child node) (car r)))
                         (progn
                           (setf (jumar:node-lchildren node) (cdr l))
                           (setf (jumar:node-child node) (car l))))))))))

;; For quota
(defun jumar:tree-set-new-root! (tree &optional node)
  "Delete nodes above NODE and set NODE to the new root of TREE.
This alikes `jumar:tree-delete-node!' with delete-below-flag t."
  (if (jumar:tree-root? tree node)
      nil
      (prog1 t
        (let1 old-root (jumar:tree-root tree)
          (unless (jumar:node<= old-root node)
            (lwarn 'jumar :error "`jumar:tree-set-new-root!': argument out of range")
            (error "`jumar:tree-set-new-root!': argument out of range"))
          (setf (jumar:tree-root tree) node)
          (setf (jumar:node-child (jumar:node-parent node)) nil)
          (setf (jumar:tree-size tree) (- (jumar:tree-size tree) (jumar:node-count-nodes old-root)))
          (if-let1 d (jumar:tree-content-destructor tree)
            (jumar:tree-for-each old-root d))
          (when (jumar:node<= old-root (jumar:tree-current tree))
            (setf (jumar:tree-current tree) (jumar:tree-root tree)))))))


;;; Auxiliary function: counting nodes
(defvar jumar:*node-count-nodes:counter* nil)
(defun jumar:node-count-nodes (node)
  (dynamic-let ((jumar:*node-count-nodes:counter* 0))
    (jumar:tree-for-each node (lambda (_) (incf jumar:*node-count-nodes:counter*)))
    jumar:*node-count-nodes:counter*))


;;; Iterator

(defun jumar:tree-for-each (tree-or-node f)
  "Apply F to the contents of TREE-OR-NODE.
If NODE-OR-TREE is node, F is applyed to subtree whose root is that node.
Oerder of application is undefined (for performance)."
  ;; Exception of the rules:
  ;;   lchildren and rchildren are processed even if child is nil.
  ;;   This case is used in `jumar:tree-delete-node!'.
  (let1 node (cond ((jumar:node-p tree-or-node) tree-or-node)
                   ((jumar:tree-p tree-or-node) (jumar:tree-root tree-or-node))
                   (t (lwarn 'jumar 'erorr "Wrong argument type: tree-or-node: %s" tree-or-node)))
    (jumar:tree-for-each:aux node f)))
(defun jumar:tree-for-each:aux (node f)
  ;; Since "list of jumarkers" is represented by tree without lchildren and rchildren,
  ;; simplest implementation may cause stack overflow.  Here we use TCO for child,
  ;; explicit stack for lchildren and rchildren.
  (erfi:let lp ((node node) (s '()) (stack '()))
    (cond (node
           (funcall f (jumar:node-content node))
           (lp (jumar:node-child node)
               s
               `(,(jumar:node-lchildren node) ,(jumar:node-rchildren node) ,@stack)))
          ((not (null s))
           (lp (car s) (cdr s) stack))
          ((not (null stack))
           (lp nil (car stack) (cdr stack)))
          (t
           t))))


;;; Conversion

(defun jumar:tree-breadcrumbs (tree &optional f)
  "Return a breadcrumb list of TREE from root to leaf along directions.

The type of return value is \"list of node-content\"."
  (let ((f (or f 'jumar:node-content))
        (root (jumar:tree-root tree)))
    (if (not root)
        '()
        (erfi:let lp ((node (jumar:tree-root tree))
                      (res '()))
          (if (jumar:node-leaf? node)
              (nreverse (cons (jumar:node-content node) res))
              (lp (jumar:node-child node) (cons (funcall f node) res)))))))

(defun jumar:tree-breadcrumbs+index (tree &optional f)
  "Return a breadcrumb list and the index of current node.

  (eq (jumar:node-content (jumar:tree-currnet tree))
      (apply 'nth (reverse (jumar:tree-breadcrums/index tree))))"
  (let ((f (or f 'jumar:node-content))
        (current (jumar:tree-current tree))
        (root (jumar:tree-root tree)))
    (if (not root)
        '(() nil)
        (erfi:let lp ((node (jumar:tree-root tree))
                      (res '())
                      (index 0)
                      (res-index nil))
          (if (jumar:node-leaf? node)
              `(,(nreverse (cons (funcall f node) res)) ,(or res-index index))
              (lp (jumar:node-child node) (cons (funcall f node) res)
                  (+ index 1) (or res-index (and (eq node current) index))))))))

(defun jumar:node->branch-string (node)
  (let ((l (jumar:node-lchildren node))
        (c (jumar:node-child node))
        (r (jumar:node-rchildren node)))
   (concat (if l "-" " ")
           (if (or l r) "+" "|")
           (if r "-" " "))))


;;; Quota

(defun jumar:tree-keep-size-limit! (tree)
  "If size is less than or equal to cadr of quota of TREE, this has no effect.
If not, reduce size by delegating to reduce-size-function of TREE or
`jumar-reduce-tree-size-function'."
  (if-let1 quota (jumar:tree-quota tree)
    (when (< (cadr quota) (jumar:tree-size tree))
      (funcall (or (jumar:tree-reduce-size-function tree) jumar-reduce-tree-size-function) tree))))

(defun jumar-default-reduce-tree-size (tree)
  "Default function of `jumar-reduce-tree-size-function'.
List up candidates by `jumar-reduce-tree-size-algorithm' and delete them.

This function has to reduce size of TREE.
Goal size is (car (jumar:tree-quota TREE)) ."
  (destructuring-bind (new-root node-list-1 node-list-2)
      (funcall jumar-reduce-tree-size-algorithm
               (jumar:tree-root tree) (car (jumar:tree-quota tree)))
    (jumar:tree-set-new-root! tree new-root)
    (dolist (node node-list-1)
      (jumar:tree-delete-node! tree node t))
    (dolist (node node-list-2)
      (jumar:tree-delete-node! tree node nil))))

;; `jumar-default-reduce-tree-size-algorithm'
;; This algorithm is simple, but has a potential problem.
;; This algorithm first find out a node (in breadcrumbs), under which all nodes are preserved.
;; Then collect branches and nodes above to be deleted.
;; Consider the following case (with appropriate limit): (Breadcrumbs of) tree is
;;
;;    |   root
;;   -+-
;;   -+-  there are many small branches
;;   -+-
;;    |
;;   -|   near leaf, there's a large branch
;;    |   leaf
;;
;; In this case, only the large branch can servive.  After deletion, tree will be the following:
;;
;;    |   root
;;    |
;;    |
;;    |
;;    |
;;   -|   near leaf, there's a large branch
;;    |   leaf
;;
;; In many cases this will be not a problem.  Although it can in the case that the current node
;; is near the place above "there are many small branches".
(defun jumar-default-reduce-tree-size-algorithm (root limit)
  "This function is called from `jumar-default-reduce-tree-size'
if `jumar-reduce-tree-size-algorithm' is untouched.

Return (node-list-1 node-list-2).  Here,
  node-list-1 is list of nodes that should be `jumar:tree-delete-node!' with below-flag t;
  node-list-2 is nodes that should be `jumar:tree-delete-node!' with below-flag nil.

After deletion, nodes below root is less than or equal to LIMIT elements.
LIMIT must be an integer greater than 2.

For example, if ROOT has less than or equal to LIMIT elements, return value is `(,root () ())."
  (progn
    (unless (and (wholenump limit) (< 2 limit))
      (lwarn 'jumar :error "argument out of range."))
    (if (>= limit (jumar:node-count-nodes root))
        ;; Trivial case.
        `(,root () ())
        ;; First, find out a node, under which all nodes preserved.
        (let1 child (jumar:node-child root)
          (erfi:let lp1 ((node child) (n nil))
            (let1 n (or n (jumar:node-count-nodes node))
              (if (< limit n)
                  ;; Go down to next node.
                  (if (and (null (jumar:node-lchildren node)) (null (jumar:node-rchildren node)))
                      (lp1 (jumar:node-child node) (- n 1))
                      (lp1 (jumar:node-child node) nil))
                  ;; Second, go up to the root and collect branches to be deleted.
                  (erfi:let lp2 ((node node)
                                 (m n)
                                 (res1 '()))
                    (if (or (eq root node) (= m limit))
                        `(,node ,res1 ())
                        (lp2 (jumar:node-parent node)
                             (+ m 1)
                             (append (jumar:node-lchildren node) (jumar:node-rchildren node) res1)))))))))))

(defun jumar-reduce-list-tree-size (tree)
  "reduce-size-function of TREE without no branches."
  (let1 new-root (let1 limit (car (jumar:tree-quota tree))
                   (erfi:let lp ((node (jumar:tree-root tree))
                                 (n (jumar:tree-size tree)))
                     (if (<= n limit)
                         node
                         (lp (jumar:node-child node) (- n 1)))))
    (jumar:tree-set-new-root! tree new-root)))



;;;
;;; Data structure: jumarker
;;;

(defstruct (jumar:jumarker
            (:constructor nil)
            (:constructor jumar:make-jumarker:aux
                          (&optional
                           (marker (point-marker))
                           (winstart (window-start))
                           &aux
                           (time (current-time))
                           (state 'available)
                           (saved-datum nil))))
  marker winstart time state saved-datum)

;; Types:
;;   marker:  Maybe Marker object of Emacs
;;   time:    Time object of Emacs (a list of three integers)  ; the created time
;;   state:   Symbol
;;   saved-datum:  Maybe List `(,buf-file-name ,point ,line ,col ,text)
;;
;; Rules:
;;   state := 'available | 'unavailable | 'reverting | 'broken
;;     'available   :  if and only if (buffur-live-p (marker-buffer marker)) is t.
;;                     (There's seconds it does not hold.)
;;     'unavailable :  if and only if the buffer is killed (or missing).
;;     'reverting   :  while the moment `before-revert-hook' between `after-revert-hook'.
;;     'broken      :  if and only if `jumar-revive-marker-function' could not revive marker.
;;   marker is nil if and only if state is 'broken.
;;   saved-datum is nil if and only if state is 'available.
;;   It is t:
;;     (let1 children (jumar:node-children node)
;;       (equal children (sort (copy-sequence children) 'jumar:jumarker-time<)))


;;; Real constructor
(defsubst jumar:make-jumarker (&optional marker winstart)
  "Make a jumaker and register it to ovservation list."
  ;; We cannot pass marker and winstart as it is to constructor of CL struct.
  ;; Probably it is due to compiler macro.  So we explicitely process optional arguments
  ;; as the constructor does.
  (rlet1 jm (jumar:make-jumarker:aux (or marker (point-marker)) (or winstart (window-start)))
    (push jm jumar:*observed-jumarker-list*)))


;;; Deletion
(defun jumar:jumarker-delete (jm)
  "Delete marker in JM.  It also unregister JM from observation list."
  (progn
    ;; TODO: use `delq'
    (setq jumar:*observed-jumarker-list* (erfi:delete jm jumar:*observed-jumarker-list* 'eq))
    (when (eq 'available (jumar:jumarker-state jm))
      (set-marker (jumar:jumarker-marker jm) nil))))


;;; State

(defun jumar:jumarker-change-state! (jm state &optional buffer)
  "Return value is undefined."
  (erfi:ecase state
    ((available)
     (let1 m (let1 sd (jumar:jumarker-saved-datum jm)
               (funcall jumar-revive-marker-function (or buffer (assq :file-path sd)) sd))
       (unless (or (markerp m) (eq m 'broken))
         (lwarn 'jumar :error
                (concat "Type error: return value of `jumar-revive-marker-function'"
                        (format " must be an available marker or symbol 'broken: %s" m))))
       (if (eq m 'broken)
           (jumar:jumarker-change-state! jm 'broken)
           (prog1 t
             (setf (jumar:jumarker-marker jm) m)
             (setf (jumar:jumarker-state jm) 'available)
             (setf (jumar:jumarker-saved-datum jm) nil)))))
    ((unavailable reverting broken)
     (when (eq 'available (jumar:jumarker-state jm))
       (setf (jumar:jumarker-saved-datum jm) (jumar:jumarker->saved-datum jm))
       (set-marker (jumar:jumarker-marker jm) nil))
     (setf (jumar:jumarker-marker jm) nil)
     (setf (jumar:jumarker-state jm) state))))


(defun jumar-default-revive-marker (buffer saved-datum)
  "Default value of `jumar-revive-marker-function'.
Try to revive a marker in BUFFER from SAVED-DATUM.
Currently, it is not so smart: It only returns maker with point (in
SAVED-DATUM).  (It is because I do not know what is the best way for
all buffers.  So I leave a room to configure by users.)

BUFFER defaults to assoc value of :file-path in SAVED-DATUM."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (cdr (assq :point saved-datum)))
      (point-marker))))


;;; Order
(defun jumar:jumarker-time< (jm1 jm2)
  (let ((t1 (jumar:jumarker-time jm1))
        (t2 (jumar:jumarker-time jm2)))
    (or (< (car t1) (car t2))
        (< (cadr t1) (cadr t2))
        (< (caddr t1) (caddr t2)))))


;;; Jump
(defun jumar:marker-goto (marker)
  (progn
;    (set-window-buffer nil (marker-buffer marker))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun jumar:jumarker-goto (jm)
  (erfi:case (jumar:jumarker-state jm)
    ((available)
     (let1 m (jumar:jumarker-marker jm)
       (jumar:marker-goto m)
       (when jumar-restore-window-start
         (set-window-start nil (jumar:jumarker-winstart jm))
         ;; Sometimes marker is not between window-start and window-end.
         ;; Making marker be in display is more important than restore window start.
         (jumar:marker-goto m))))
    ((unavailable)
     (unless (and-let* ((file-path (cdr-safe (assq :file-path (jumar:jumarker-saved-datum jm))))
                        (buf (funcall jumar-reopen-file-function file-path)))
               (prog1 t
                 ;; Now, state of JM should be changed by call of `jumar:jumarker-hook-find-file'.
                 ;; (if (eq 'available (jumar:jumarker-state jm))
                 ;;     (jumar:jumarker-goto jm)
                 ;;     (lwarn 'jumar :error "`jumar-reopen-file-function' violate the rule."))))
                 (erfi:case (jumar:jumarker-state jm)
                   ((available) (jumar:jumarker-goto jm))
                   ((broken)    (jumar:message "Reopen failed and unable to jump."))
                   (else        (lwarn 'jumar :error "`jumar-reopen-file-function' violate the rule.")))))
       (jumar:message "Reopen failed and unable to jump.")))
    (else
     (lwarn 'jumar :error "Unable to jump.  Unexpected case: %S" (jumar:jumarker-state jm)))))


;;; Conversion
(defun jumar:jumarker->position-datum (jm)
  "[internal] Return a list `(,buf ,pos ,winstart ,buf-name ,line ,column ,text) that JM indicates.

Note that this is an internal function.  The order and elements may be changed.
For extensions, use `jumar:jumarker->saved-datum'."
  (if (not (eq 'available (jumar:jumarker-state jm)))
      (let* ((sd (jumar:jumarker-saved-datum jm))
             (file-path (cdr-safe (assq :file-path sd)))
             (buf-name (cdr-safe (assq :buffer-name sd))))
        `(nil
          ,@(mapcar (erfi:$ cdr $ assq <> sd $) '(:point :winstart))
          ,(if file-path (file-name-nondirectory file-path) buf-name)
          ,@(mapcar (erfi:$ cdr $ assq <> sd $) '(:line :column))
          ,(concat (propertize "Killed buffer" 'font-lock-face '(:background "gray20"))
                   " " file-path)))
      (let* ((m (jumar:jumarker-marker jm))
             (buf (marker-buffer m))
             (buf-name (buffer-name buf))
             (pos (marker-position m)))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char pos)
              (let* ((line (line-number-at-pos pos))
                     (column (- pos (point-at-bol)))
                     (text (if (null jumar-preview-text-length-limit)
                               ""
                               (erfi:$
                                funcall jumar-preview-text-process-function <> column $
                                buffer-substring (point-at-bol) $
                                min (point-at-eol) (+ (point-at-bol) jumar-preview-text-length-limit)))))
                (list buf pos (jumar:jumarker-winstart jm) buf-name line column text))))))))

(defun jumar:jumarker->saved-datum (jm)
  "Return an alist of datum.
Calling this function is allowed only if JM is available.
Keys are :file-path, :buffer-name, :point, :winstart, :line, :column."
  (destructuring-bind (buf pos winstart buf-name line column _) (jumar:jumarker->position-datum jm)
    `((:file-path . ,(buffer-file-name buf))
      (:buffer-name . ,(buffer-name buf))
      (:point . ,pos)
      (:winstart . ,winstart)
      (:line . ,line)
      (:column . ,column))))


;;; Hooks
(defvar jumar:*observed-jumarker-list* '()
  "List of jumarkers observed to guarantee states of markers are correct.
Kill or revert buffers makes those unavailable.  When file is reopend,
revive jumarkers in it.")

(defun jumar:jumarker-hook-kill-buffer ()
  "This guarantees state of jumarkers are correct."
  (let1 buf (current-buffer)
    (dolist (jm jumar:*observed-jumarker-list*)
      (if-let1 marker (jumar:jumarker-marker jm)
        (when (eq buf (marker-buffer marker))
          (jumar:jumarker-change-state! jm 'unavailable))))))
(add-hook 'kill-buffer-hook 'jumar:jumarker-hook-kill-buffer)

(defun jumar:jumarker-hook-before-revert ()
  "This guarantees state of jumarkers are correct."
  (let1 buf (current-buffer)
    (dolist (jm jumar:*observed-jumarker-list*)
      (if-let1 marker (jumar:jumarker-marker jm)
        (when (eq buf (marker-buffer marker))
          (jumar:jumarker-change-state! jm 'reverting))))))
(defun jumar:jumarker-hook-find-file ()
  "This guarantees state of jumarkers are correct."
  (let1 buf (current-buffer)
    (dolist (jm jumar:*observed-jumarker-list*)
      (when (and (not (eq 'available (jumar:jumarker-state jm)))
                 (string= (cdr (assq :file-path (jumar:jumarker-saved-datum jm)))
                          (or (buffer-file-name buf) (buffer-name buf))))
        (jumar:jumarker-change-state! jm 'available (current-buffer))))))
(add-hook 'before-revert-hook 'jumar:jumarker-hook-before-revert)
(add-hook 'find-file-hook 'jumar:jumarker-hook-find-file)



;;;
;;; Core
;;;

(defvar jumar:*jm-tree* (jumar:make-tree 'jumar:jumarker-delete))
(defvar jumar:*jm-list* (jumar:make-tree 'jumar:jumarker-delete nil 'jumar-reduce-list-tree-size))

(defun jumar-init ()
  "Initialize internal variables.
User has to call this function after modifying variables below:

  `helm-jumar-candidate-number-limit'
  `helm-jumar-map'"
  (progn
    (setq *helm-jumar-candidate-number-limit*
          (let1 n-or-list helm-jumar-candidate-number-limit
            (cond ((null n-or-list)
                   n-or-list)
                  ((integerp n-or-list)
                   `(,n-or-list ,n-or-list))
                  ((and (listp n-or-list)
                        (= 2 (length n-or-list)))
                   n-or-list)
                  (t
                   (error "Wrong type: `helm-jumar-candidate-number-limit'")))))
    (when (featurep 'helm)
      (helm-jumar:reset-sourecs))))

(defun jumar-default-reopen-file (file-path)
  (if (funcall jumar-y-or-n-p (format "Reopen %s?" file-path))
      (find-file file-path)
      (prog1 nil
        (jumar:message "File is not reopend."))))



;;;
;;; Very simple UI for jumar:*jm-tree*
;;;

(defvar jumar-post-jump-hook nil
  "Hooks run after jump.")

;;; Add marker
;; Commands with last "*" change the current (add marker to the tree and change current node to that).

(defun jumar:message (&rest args)
  (apply jumar-message-function args))

(defun jumar-add-marker ()
  (interactive)
  (progn
    (jumar:tree-enhance-downward! jumar:*jm-tree* (jumar:make-jumarker))
;    (jumar:buffer-draw)
    (jumar:message "Add marker.")))
(defun jumar-add-marker* ()
  (interactive)
  (progn
    (jumar:tree-enhance-downward! jumar:*jm-tree* (jumar:make-jumarker) t)
;    (jumar:buffer-draw)
    (jumar:message "Add marker.")))

(defun jumar-add-marker/last ()
  (interactive)
  (progn
    (jumar:tree-enhance-last! jumar:*jm-tree* (jumar:make-jumarker) nil)
;    (jumar:buffer-draw)
    (jumar:message "Add marker.")))

(defun jumar-add-marker/last* ()
  (interactive)
  (progn
    (jumar:tree-enhance-last! jumar:*jm-tree* (jumar:make-jumarker) t)
;    (jumar:buffer-draw)
    (jumar:message "Add marker.")))


;;; Jump

(defun jumar-jump-current:aux (jm-set)
  (progn
    (if-let1 current (jumar:tree-current jm-set)
      (jumar:jumarker-goto (jumar:node-content current))
      (jumar:message "No marker."))
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-jump-current ()
  (interactive)
  (jumar-jump-current:aux jumar:*jm-tree*))

(defun jumar-jump-forward ()
  (interactive)
  (progn
    (if-let1 current (jumar:tree-descend-current! jumar:*jm-tree*)
      (jumar:jumarker-goto (jumar:node-content current))
      (progn
        (when jumar-jump-current-if-no-further-marker
          (jumar:jumarker-goto (jumar:node-content (jumar:tree-current jumar:*jm-tree*))))
        (jumar:message "No more marker forward.")))
    (run-hooks 'jumar-post-jump-hook)))

(defun jumar-jump-backward ()
  (interactive)
  (progn
    (if-let1 current (jumar:tree-ascend-current! jumar:*jm-tree*)
      (jumar:jumarker-goto (jumar:node-content current))
      (progn
        (when jumar-jump-current-if-no-further-marker
          (jumar:jumarker-goto (jumar:node-content (jumar:tree-current jumar:*jm-tree*))))
        (jumar:message "No more marker backward.")))
    (run-hooks 'jumar-post-jump-hook)))



;;;
;;; Jumar with Helm
;;;

;;; Auxiliary function: update helm first node preserved

(defun helm-jumar-force-update ()
  (progn
    (unless *helm-jumar-tree-next-candidate-focus*
      (setq *helm-jumar-tree-next-candidate-focus*
            `(first ,(cdar *helm-jumar-jumarkers-tree-cache*))))
    (unless *helm-jumar-list-next-candidate-focus*
      (setq *helm-jumar-list-next-candidate-focus*
            `(first ,(cdar *helm-jumar-jumarkers-list-cache*))))
    (helm-force-update)))


;;; Auxiliary functions: get appropriate set

(defun helm-jumar:source->jm-set-type (source)
  ;; Is there a better way?
  (let1 name (helm-attr 'name source)
    (cond ((string= name "Jumarkers in tree") 'tree)
          ((string= name "Jumarkers in list") 'list)
          (t
           (lwarn 'jumar :error "`helm-jumar:source->jm-set-type': Unkown source.")
           (error "`helm-jumar:source->jm-set-type': Unkown source.")))))

(defun helm-jumar:source->jm-set (source)
  (erfi:ecase (helm-jumar:source->jm-set-type source)
    ((tree) jumar:*jm-tree*)
    ((list) jumar:*jm-list*)))

(defun helm-jumar:get-set-containing-node (node)
  (let1 jm-sets (mapcar 'helm-jumar:source->jm-set (helm-get-sources))
    (loop for s in jm-sets
          when (jumar:tree-has-node? s node)
          return s
          finally (progn
                    (lwarn 'jumar :error "`helm-jumar:get-set-containing-node': source not found")
                    (error "`helm-jumar:get-set-containging-node': source not found")))))


;;; Auxiliary functions: preselect with real

(defun helm-jumar:preselect-with-real (real &optional elt= force-having-real)
  (if-let1 p (helm-jumar:get-point-with-real real elt= force-having-real)
    (with-helm-window
      (goto-char p)
      (helm-mark-current-line))
    (lwarn 'jumar :error
           "helm-jumar:preselect-with-real: (DISPLAY . REAL) pair with designation not found.")))

(defun helm-jumar:get-point-with-real (real &optional elt= force-having-real)
  (let1 a (helm-jumar:get-point-list-where-real-exist real elt= force-having-real)
    (if (listp a)
        (car a)
        nil)))

(defun helm-jumar:get-point-list-where-real-exist (real &optional elt= force-having-real)
  "Return a list of points in helm-buffer, whose real part of data is equal to REAL.

If FORCE-HAVING-REAL is t, only list up points of the form (DISPLAY' . REAL') pairs.
Otherwise, strings DISPLAY' are also considered.

If ELT= is non-nil, it is used for equality test.  It defaults to `string='.
This function is called as the following:
  (ELT= REAL REAL')     for paris (DISPLAY' . REAL')
  (ELT= REAL DISPLAY')  for strings DISPLAY'
If FORCE-HAVING-REAL is t, the later case does not occur.
So, checking string or not is necessary if FORCE-HAVING-REAL is nil and REAL are
not string."
  (let* ((elt= (or elt= 'string=))
         (candidates (erfi:$ mapcar (lambda (x) (if (not (stringp x)) x `(,x . ,x)))
                             $ erfi:filter (if force-having-real
                                               (erfi:complement 'stringp)
                                               (cut 'identity t))
                             $ erfi:concatenate $ mapcar 'helm-get-candidates $ helm-get-sources))
         (matched (erfi:filter (erfi:$ funcall elt= real $ cdr $) candidates)))
    (with-helm-buffer
      (save-excursion
        (erfi:let lp1 ((matched matched) (res '()))
          (if (null matched)
              (sort res '<)
              (destructuring-bind (d . r) (car matched)
                (erfi:let lp2 ((p (point-min)) (res res))
                  (goto-char p)
                  (if (not (search-forward d nil t))
                      (lp1 (cdr matched) res)
                      (lp2 (match-end 0)
                           (progn
                             (if (funcall elt= real
                                          (get-text-property (match-beginning 0) 'helm-realvalue))
                                 (cons (match-beginning 0) res)
                                 res))))))))))))


;;; Making candidate list

(defun helm-jumar:make-candidates (tree &optional focus-type focused-node)
  "FOCUS-TYPE := 'first | 'center | nil"
  (jumar:d-let*
      (( (n-list c-index)
         (jumar:tree-breadcrumbs+index tree 'identity) )
       ( (node-list current-index)
         (if (null n-list)
             `(,n-list nil)
             (if (not *helm-jumar-candidate-number-limit*)
                 `(,n-list ,c-index)
                 (destructuring-bind (a b) *helm-jumar-candidate-number-limit*
                   (let* ((focus-index (or (and focus-type
                                                (erfi:list-index (cut eq focused-node <>) n-list))
                                           c-index))
                          (d (erfi:case focus-type
                               ((first)      focus-index)
                               ((center nil) (max 0 (- focus-index (+ a 1)))))))
                     `(,(erfi:$ erfi:take* <> (+ a b 1) $ erfi:drop n-list d)
                       ,(- c-index d)))))) )
       ( (buf-name-list line-list column-list text-list)
         (erfi:unzip4 (mapcar (erfi:$ cdddr $ jumar:jumarker->position-datum $ jumar:node-content $)
                              node-list)) ))
    (let ((indicator-list  (if (wholenump current-index)
                               `(,@(make-list current-index " ") ">" ,@(erfi:circular-list " "))
                               (erfi:circular-list " ")))
          (branch-str-list (mapcar 'jumar:node->branch-string node-list))
          (line-list*      (erfi:normalize-strings-length (mapcar 'number-to-string line-list)
                                                          'right 4))
          (column-list*    (erfi:normalize-strings-length (mapcar 'number-to-string column-list)
                                                          'right 3))
          (buf-name-list*  (erfi:normalize-strings-length buf-name-list 'left 8))
          (padding         (erfi:circular-list "  ")))
      (let1 str-list (erfi:map 'concat
                               indicator-list
                               padding branch-str-list
                               padding line-list*
                               padding column-list*
                               padding buf-name-list*
                               padding text-list)
        (erfi:map 'cons str-list node-list)))))

;;; Sources
(eval-after-load "helm"
  '(progn
     (defvar *helm-jumar-candidate-number-limit* nil "[internal] Initialized by `jumar-init'.")
     (defvar *helm-jumar-jumarkers-tree-cache* nil)
     (defvar *helm-jumar-jumarkers-list-cache* nil)
     (defvar *helm-jumar-tree-next-candidate-focus* nil)
     (defvar *helm-jumar-list-next-candidate-focus* nil)

     (defvar helm-jumar-map
       (rlet1 map (make-sparse-keymap)
         (set-keymap-parent map helm-map)
         (define-key map (kbd "M-f") 'helm-jumar-run-forward-branch)
         (define-key map (kbd "M-b") 'helm-jumar-run-backward-branch)
         (define-key map (kbd "C-r") 'helm-jumar-run-recenter)
         (define-key map (kbd "C-c d") 'helm-jumar-run-delete-nodes-below)
         (define-key map (kbd "C-c k") 'helm-jumar-run-delete-marked-nodes)
         ))

     (defclass helm-type-jumarker (helm-source) ()
       "A class to define type jumar.")

     (defmethod helm--setup-source :before ((source helm-type-jumarker))
       (oset source :action (helm-make-actions
                             "Jump to marker" 'helm-jumar-jump/set-current
                             "Delete marker(s)" 'helm-jumar-delete-marked-nodes
                             "Delete markers below" 'helm-jumar-delete-nodes-below
                             "Forward branch" 'helm-jumar-forward-branch
                             "Backward branch" 'helm-jumar-backward-branch
                             ))
       (oset source :persistent-help "Peep this marker.")
       (oset source :filtered-candidate-transformer '()))

     (defclass helm-source-jumarkers-tree-recipe (helm-source-sync helm-type-jumarker)
       ((init :initform (lambda ()
                          (setq *helm-jumar-jumarkers-tree-cache*
                                (apply 'helm-jumar:make-candidates
                                       jumar:*jm-tree* *helm-jumar-tree-next-candidate-focus*))
                          (setq *helm-jumar-tree-next-candidate-focus* nil)))
        (candidates :initform *helm-jumar-jumarkers-tree-cache*)
        (matchplugin :initform nil)
        (persistent-action :initform 'helm-jumar-persistent-action)
        (keymap :initform helm-jumar-map)
        (persistent-help
         :initform
         "Peep this jumarker / C-u \\[helm-execute-persistent-action]: Delete this jumarker")))

     (defclass helm-source-jumarkers-list-recipe (helm-source-sync helm-type-jumarker)
       ((init :initform (lambda ()
                          (setq *helm-jumar-jumarkers-list-cache*
                                (apply 'helm-jumar:make-candidates
                                       jumar:*jm-list* *helm-jumar-list-next-candidate-focus*))
                          (setq *helm-jumar-list-next-candidate-focus* nil)))
        (candidates :initform *helm-jumar-jumarkers-list-cache*)
        (matchplugin :initform nil)
        (persistent-action :initform 'helm-jumar-persistent-action)
        (keymap :initform helm-jumar-map)
        (persistent-help
         :initform
         "Peep this jumarker / C-u \\[helm-execute-persistent-action]: Delete this jumarker")))

     ;; These names are very important in the current implementation.
     ;; See `helm-jumar:source->jm-set-type'.
     (defvar helm-source-jumarkers-tree nil)
     (defvar helm-source-jumarkers-list nil)

     (defun helm-jumar:reset-sourecs ()
       (progn
         (setq helm-source-jumarkers-tree
               (helm-make-source "Jumarkers in tree" 'helm-source-jumarkers-tree-recipe))
         (setq helm-source-jumarkers-list
               (helm-make-source "Jumarkers in list" 'helm-source-jumarkers-list-recipe))))
     ))


;;; Fundamental functions called in Helm session

(defun helm-jumar-jump/set-current (node)
  "Jump to selected jumarker and set NODE to the current node."
  (let1 jm-set (helm-jumar:source->jm-set (helm-get-current-source))
    (setf (jumar:tree-current jm-set) node)
    (jumar-jump-current:aux jm-set)))

(defun helm-jumar-persistent-action (node)
  "Peep jumarker of NODE, or delete it if called with prefix argument."
  (if (not current-prefix-arg)
      (progn
        (jumar:jumarker-goto (jumar:node-content node))
        (helm-highlight-current-line))
      (let1 next-preselect (or (jumar:node-child node) (jumar:node-parent node))
        (jumar:tree-delete-node! (helm-jumar:get-set-containing-node node) node)
        (helm-jumar-force-update)
        (when next-preselect
          (helm-jumar:preselect-with-real next-preselect 'eq t))
        (jumar:message "1 jumarker deleted."))))

(defun helm-jumar-delete-marked-nodes (_)
  "Delete selected jumarkers."
  ;; This is not very efficient.
  ;; We can make it more fast if there is a method to obtain a source from a candidate.
  (let* ((nodes (helm-marked-candidates))
         (len (length nodes))
         (jm-sets (mapcar 'helm-jumar:source->jm-set (helm-get-sources))))
    (dolist (node nodes)
      (jumar:tree-delete-node! (helm-jumar:get-set-containing-node node) node))
    (jumar:message "%s jumarker(s) deleted." len)))

(defun helm-jumar-delete-nodes-below (node)
  "Delete nodes below NODE."
  (progn
    (let1 n (jumar:node-count-nodes node)
      (jumar:tree-delete-node! (helm-jumar:get-set-containing-node node) node t)
      (jumar:message "%s jumarker(s) deleted." n))))

;;; Changing branch and recenter

(defun helm-jumar-forward-branch:aux (node direction)
  (progn
    (jumar:node-forward-branch! node direction)
    ;; TODO
    ;; Invoking `helm-quit' after changing branch can violate the rule of `jumar:tree'
    ;; "breadcrumb list must contain current".  To avoid this, we update current
    ;; to the selected node.  This also will be solved by restorinig original jm-sets
    ;; when `helm-quit' is invoked, but we need more work under presence of `helm-resume'.
    (setf (jumar:tree-current (helm-jumar:get-set-containing-node node)) node)
    (helm-jumar-force-update)
    (helm-jumar:preselect-with-real node 'eq t)))

(defun helm-jumar-forward-branch (node)
  "Change branch to the next one if exists.
Note that this change is not temporary in Helm session."
  (progn
    (helm-jumar-forward-branch:aux node +1)
    (jumar:message "Branch forwarded.")))

(defun helm-jumar-backward-branch (node)
  "Change branch to the previous one if exists.
Note that this change is not temporary in Helm session."
  (progn
    (helm-jumar-forward-branch:aux node -1)
    (jumar:message "Branch backwarded.")))

(defun helm-jumar-recenter (node)
  (progn
    (erfi:ecase (helm-jumar:source->jm-set-type (helm-get-current-source))
      ((tree) (setq *helm-jumar-tree-next-candidate-focus* `(center ,node)))
      ((list) (setq *helm-jumar-list-next-candidate-focus* `(center ,node))))
    (helm-jumar-force-update)
    (helm-jumar:preselect-with-real node 'eq t)
    (jumar:message "Recentered.")))

(defun helm-jumar-run-forward-branch ()
  "Change branch of selected node forward."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'change-branch-action '(helm-jumar-forward-branch . never-split))
    (helm-execute-persistent-action 'change-branch-action)))

(defun helm-jumar-run-backward-branch ()
  "Change branch of selected node backward."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'change-branch-action '(helm-jumar-backward-branch . never-split))
    (helm-execute-persistent-action 'change-branch-action)))

(defun helm-jumar-run-recenter ()
  "Recenter to the selected node."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'recenter-action '(helm-jumar-recenter . never-split))
    (helm-execute-persistent-action 'recenter-action)))

(defun helm-jumar-run-delete-marked-nodes ()
  "Delete selected nodes without quitting."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'delete-action '(helm-jumar-delete-marked-nodes . never-split))
    (helm-execute-persistent-action 'delete-action)
    ;; Note that this update is "incorrect" when one invoked recenter and delete "top" node.
    (helm-jumar-force-update)))

(defun helm-jumar-run-delete-nodes-below ()
  "Delete nodes below selected one without quitting."
  (interactive)
  (with-helm-alive-p
    (let1 next-preselect (jumar:node-parent (helm-get-selection))
      (helm-attrset 'delete-action '(helm-jumar-delete-nodes-below . never-split))
      (helm-execute-persistent-action 'delete-action)
      ;; Note that this update is "incorrect" when one invoked recenter and delete "top" node.
      (helm-jumar-force-update)
      (when next-preselect
        (helm-jumar:preselect-with-real next-preselect 'eq t)))))


;;; Preconfigured Helm interface

(defun helm-jumar-jumarkers ()
  "Preconfigured `helm' for jump to jumarkers."
  (interactive)
  (helm :sources '(helm-source-jumarkers-tree)
        :buffer "*helm jumar*"
        :keymap helm-jumar-map
        :preselect "^>"
        :truncate-lines t))



;;;
;;; Initialization for ordinary users
;;;

(jumar-init)


(provide 'jumar)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; jumar.el ends here
