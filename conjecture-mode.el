;;; conjecture-mode.el --- Minor mode for Clojure tests with Conjecture

;; Copyright © 2013 Paul Stadig
;; Copyright © 2009-2011 Phil Hagelberg

;; Author: Paul Stadig <paul@stadig.name>
;; URL: http://emacswiki.org/cgi-bin/wiki/ConjectureMode
;; Version: 1.0
;; Keywords: languages, lisp, test
;; Package-Requires: ((clojure-mode "1.7") (nrepl "0.1.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; conjecture.core framework) via nrepl.el and seeing feedback in the test
;; buffer about which tests failed or errored.

;;; Usage:

;; Once you have an nrepl session active, you can run the tests in the
;; current buffer with C-c C-,. Failing tests and errors will be
;; highlighted using overlays. To clear the overlays, use C-c k.

;; You can jump between implementation and test files with <kbd>C-c C-t</kbd> if
;; your project is laid out in a way that conjecture-mode expects. Your
;; project root should have a `src/` directory containing files that
;; correspond to their namespace. It should also have a `test/` directory
;; containing files that correspond to their namespace, and the test
;; namespaces should mirror the implementation namespaces with the
;; addition of "-test" as the suffix to the last segment of the namespace.

;; So `my.project.frob` would be found in `src/my/project/frob.clj` and
;; its tests would be in `test/my/project/frob_test.clj` in the
;; `my.project.frob-test` namespace.

;;; History:

;; 1.0: 2013-03-22
;;  * Initial fork from clojure-test-mode

;;; TODO:

;; * Prefix arg to jump-to-impl should open in other window
;; * Put Testing indicator in modeline while tests are running
;; * Integrate with M-x next-error
;; * Error messages need line number.
;; * Currently show-message needs point to be on the line with the
;;   "is" invocation; this could be cleaned up.

;;; Code:

(require 'cl)
(require 'clojure-mode)
(require 'which-func)
(require 'nrepl)

(declare-function nrepl-repl-buffer            "nrepl.el")
(declare-function nrepl-make-response-handler  "nrepl.el")
(declare-function nrepl-send-string            "nrepl.el")
(declare-function nrepl-current-ns             "nrepl.el")
(declare-function nrepl-current-tooling-session "nrepl.el")
(declare-function nrepl-current-connection-buffer "nrepl.el")

;; Faces

(defface conjecture-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over this.
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in Clojure tests."
  :group 'conjecture-mode)

(defface conjecture-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in Clojure tests."
  :group 'conjecture-mode)

(defface conjecture-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for success in Clojure tests."
  :group 'conjecture-mode)

;; Counts

(defvar conjecture-count 0)
(defvar conjecture-failure-count 0)
(defvar conjecture-error-count 0)

;; Consts

(defconst conjecture-ignore-results
  '(:end-test-ns :begin-test-var :end-test-var)
  "Results from test-is that we don't use")

;; Support Functions

(defun conjecture-nrepl-connected-p ()
  (nrepl-current-connection-buffer))

(defun conjecture-make-handler (callback)
  (lexical-let ((buffer (current-buffer))
                (callback callback))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (funcall callback buffer value))
                                 (lambda (buffer value)
                                   (nrepl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (nrepl-emit-interactive-output err))
                                 '())))

(defun conjecture-eval (string &optional handler)
  (nrepl-send-string string
                     (conjecture-make-handler (or handler #'identity))
                     (or (nrepl-current-ns) "user")
                     (nrepl-current-tooling-session)))

(defun conjecture-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (when (conjecture-nrepl-connected-p)
    (nrepl-send-string-sync
     "(ns conjecture.mode
        (:use [conjecture.core :only [file-position *testing-vars* *test-out*
                                   join-fixtures *report-counters* do-report
                                   test-var *initial-report-counters*]]
              [clojure.pprint :only [pprint]]))

    (def #^{:dynamic true} *conjecture-mode-out* nil)
    (def fail-events #{:fail :error})
    (defn report [event]
     (if-let [current-test (last conjecture.core/*testing-vars*)]
        (alter-meta! current-test
                     assoc :status (conj (:status (meta current-test))
                                     [(:type event)
                                      (:message event)
                                      (when (fail-events (:type event))
                                        (str (:expected event)))
                                      (when (fail-events (:type event))
                                        (str (:actual event)))
                                      (case (:type event)
                                        :fail (with-out-str (pprint (:actual event)))
                                        :error (with-out-str
                                                (clojure.stacktrace/print-cause-trace
                                                (:actual event)))
                                        nil)
                                      (if (and (= (:major *clojure-version*) 1)
                                               (< (:minor *clojure-version*) 2))
                                          ((file-position 2) 1)
                                          (if (= (:type event) :error)
                                              ((file-position 3) 1)
                                              (:line event)))])))
     (binding [*test-out* (or *conjecture-mode-out* *out*)]
       ((.getRawRoot #'conjecture.core/report) event)))

   (defn conjecture-mode-test-one-var [test-ns test-name]
     (let [v (ns-resolve test-ns test-name)
           once-fixture-fn (join-fixtures (::once-fixtures (meta (find-ns test-ns))))
           each-fixture-fn (join-fixtures (::each-fixtures (meta (find-ns test-ns))))]
       (once-fixture-fn
        (fn []
          (when (:test (meta v))
            (each-fixture-fn (fn [] (test-var v))))))))

    ;; adapted from test-ns
    (defn conjecture-mode-test-one-in-ns [ns test-name]
      (binding [*report-counters* (ref *initial-report-counters*)]
        (let [ns-obj (the-ns ns)]
          (do-report {:type :begin-test-ns, :ns ns-obj})
          ;; If the namespace has a test-ns-hook function, call that:
          (if-let [v (find-var (symbol (str (ns-name ns-obj)) \"test-ns-hook\"))]
            ((var-get v))
            ;; Otherwise, just test every var in the namespace.
            (conjecture-mode-test-one-var ns test-name))
          (do-report {:type :end-test-ns, :ns ns-obj}))
        (do-report (assoc @*report-counters* :type :summary))))"
     (or (nrepl-current-ns) "user")
     (nrepl-current-tooling-session))))

(defun conjecture-get-results (buffer result)
  (with-current-buffer buffer
    (conjecture-eval
     (concat "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns '"
             (clojure-find-ns) ")))")
     #'conjecture-extract-results)))

(defun conjecture-extract-results (buffer results)
  (with-current-buffer buffer
    (let ((result-vars (read results)))
      (mapc #'conjecture-extract-result result-vars)
      (conjecture-echo-results))))

(defun conjecture-extract-result (result)
  "Parse the result from a single test. May contain multiple is blocks."
  (dolist (is-result (rest result))
    (unless (member (aref is-result 0) conjecture-ignore-results)
      (incf conjecture-count)
      (destructuring-bind (event msg expected actual pp-actual line)
          (coerce is-result 'list)
        (if (equal :fail event)
            (progn (incf conjecture-failure-count)
                   (conjecture-highlight-problem
                    line event (format "Expected %s, got %s" expected actual)
                    pp-actual))
          (when (equal :error event)
            (incf conjecture-error-count)
            (conjecture-highlight-problem
             line event actual pp-actual))))))
  (conjecture-echo-results))

(defun conjecture-echo-results ()
  (message
   (propertize
    (format "Ran %s tests. %s failures, %s errors."
            conjecture-count conjecture-failure-count
            conjecture-error-count)
    'face
    (cond ((not (= conjecture-error-count 0)) 'conjecture-error-face)
          ((not (= conjecture-failure-count 0)) 'conjecture-failure-face)
          (t 'conjecture-success-face)))))

(defun conjecture-highlight-problem (line event message pp-actual)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((beg (point)))
      (end-of-line)
      (let ((overlay (make-overlay beg (point))))
        (overlay-put overlay 'face (if (equal event :fail)
                                       'conjecture-failure-face
                                     'conjecture-error-face))
        (overlay-put overlay 'message message)
        (overlay-put overlay 'actual pp-actual)))))

;; Problem navigation
(defun conjecture-find-next-problem (here)
  "Go to the next position with an overlay message.
Retuns the problem overlay if such a position is found, otherwise nil."
  (let ((current-overlays (overlays-at here))
        (next-overlays (next-overlay-change here)))
    (while (and (not (equal next-overlays (point-max)))
                (or
                 (not (overlays-at next-overlays))
                 (equal (overlays-at next-overlays)
                        current-overlays)))
      (setq next-overlays (next-overlay-change next-overlays)))
    (if (not (equal next-overlays (point-max)))
        (overlay-start (car (overlays-at next-overlays))))))

(defun conjecture-find-previous-problem (here)
  "Go to the next position with the `conjecture-problem' text property.
Retuns the problem overlay if such a position is found, otherwise nil."
  (let ((current-overlays (overlays-at here))
        (previous-overlays (previous-overlay-change here)))
    (while (and (not (equal previous-overlays (point-min)))
                (or
                 (not (overlays-at previous-overlays))
                 (equal (overlays-at previous-overlays)
                        current-overlays)))
      (setq previous-overlays (previous-overlay-change previous-overlays)))
    (if (not (equal previous-overlays (point-min)))
        (overlay-start (car (overlays-at previous-overlays))))))

;; File navigation

(defun conjecture-implementation-for (namespace)
  "Returns the path of the src file for the given test namespace."
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (namespace-end (split-string (car (last segments)) "_"))
         (namespace-end (mapconcat 'identity (butlast namespace-end 1) "_"))
         (impl-segments (append (butlast segments 1) (list namespace-end))))
    (format "%s/src/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity impl-segments "/"))))

(defvar conjecture-implementation-for-fn 'conjecture-implementation-for
  "Var pointing to the function that will return the full path of the
Clojure src file for the given test namespace.")

;; Commands

(defun conjecture-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing...")
  (if (not (clojure-in-tests-p))
      (nrepl-load-file (buffer-file-name)))
  (save-window-excursion
    (if (not (clojure-in-tests-p))
        (clojure-jump-to-test))
    (conjecture-clear)
    (conjecture-eval (format "(binding [conjecture.core/report conjecture.mode/report]
                                       (conjecture.core/run-tests '%s))"
                               (clojure-find-ns))
                       #'conjecture-get-results)))

(defun conjecture-run-test ()
  "Run the test at point."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (imenu--make-index-alist)
  (conjecture-clear)
  (let* ((f (which-function))
         (test-name (if (listp f) (first f) f)))
    (conjecture-eval (format "(binding [conjecture.core/report conjecture.mode/report]
                                  (load-file \"%s\")
                                  (conjecture.mode/conjecture-mode-test-one-in-ns '%s '%s)
                                  (cons (:name (meta (var %s))) (:status (meta (var %s)))))"
                               (buffer-file-name) (clojure-find-ns)
                               test-name test-name test-name)
                       (lambda (buffer result-str)
                         (with-current-buffer buffer
                           (let ((result (read result-str)))
                             (if (cdr result)
                                 (conjecture-extract-result result)
                               (message "Not in a test."))))))))

(defun conjecture-show-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defun conjecture-pprint-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (when overlay
      (with-current-buffer (generate-new-buffer " *test-output*")
        (buffer-disable-undo)
        (insert (overlay-get overlay 'actual))
        (switch-to-buffer-other-window (current-buffer))))))

;;; ediff results
(defvar conjecture-ediff-buffers nil)

(defun conjecture-ediff-cleanup ()
  "A function for ediff-cleanup-hook, to cleanup the temporary ediff buffers"
  (mapc (lambda (b) (when (get-buffer b) (kill-buffer b)))
        conjecture-ediff-buffers))

(defun conjecture-ediff-result ()
  "Show the result of the test under point as an ediff"
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (let* ((m (overlay-get overlay 'actual)))
          (let ((tmp-buffer (generate-new-buffer " *conjecture-mode-tmp*"))
                (exp-buffer (generate-new-buffer " *expected*"))
                (act-buffer (generate-new-buffer " *actual*")))
            (with-current-buffer tmp-buffer
              (insert m)
              (clojure-mode)
              (goto-char (point-min))
              (forward-char) ; skip a paren
              (paredit-splice-sexp) ; splice
              (lexical-let ((p (point))) ; delete "not"
                (forward-sexp)
                (delete-region p (point)))
              (lexical-let ((p (point))) ; splice next sexp
                (forward-sexp)
                (backward-sexp)
                (forward-char)
                (paredit-splice-sexp))
              (lexical-let ((p (point))) ; delete operator
                (forward-sexp)
                (delete-region p (point)))
              (lexical-let ((p (point))) ; copy first expr
                (forward-sexp)
                (lexical-let ((p2 (point)))
                  (with-current-buffer exp-buffer
                    (insert-buffer-substring-as-yank tmp-buffer (+ 1 p) p2))))
              (lexical-let ((p (point))) ; copy next expr
                (forward-sexp)
                (lexical-let ((p2 (point)))
                  (with-current-buffer act-buffer
                    (insert-buffer-substring-as-yank tmp-buffer (+ 1 p) p2)))))
            (kill-buffer tmp-buffer)
            (setq conjecture-ediff-buffers
                  (list (buffer-name exp-buffer) (buffer-name act-buffer)))
            (ediff-buffers
             (buffer-name exp-buffer) (buffer-name act-buffer)))))))

(defun conjecture-load-current-buffer ()
  (let ((command (format "(clojure.core/load-file \"%s\")\n(in-ns '%s)"
                         (buffer-file-name)
                         (clojure-find-ns))))
    (nrepl-send-string-sync command)))

(defun conjecture-clear (&optional callback)
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (setq conjecture-count 0
        conjecture-failure-count 0
        conjecture-error-count 0)
  (conjecture-load-current-buffer))

(defun conjecture-next-problem ()
  "Go to and describe the next test problem in the buffer."
  (interactive)
  (let* ((here (point))
         (problem (conjecture-find-next-problem here)))
    (if problem
        (goto-char problem)
      (goto-char here)
      (message "No next problem."))))

(defun conjecture-previous-problem ()
  "Go to and describe the previous compiler problem in the buffer."
  (interactive)
  (let* ((here (point))
         (problem (conjecture-find-previous-problem here)))
    (if problem
        (goto-char problem)
      (goto-char here)
      (message "No previous problem."))))

(defun conjecture-jump-to-implementation ()
  "Jump from test file to implementation."
  (interactive)
  (find-file (funcall conjecture-implementation-for-fn
                      (clojure-find-package))))

(defvar conjecture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'conjecture-run-tests)
    (define-key map (kbd "C-c ,")   'conjecture-run-tests)
    (define-key map (kbd "C-c M-,") 'conjecture-run-test)
    (define-key map (kbd "C-c C-'") 'conjecture-ediff-result)
    (define-key map (kbd "C-c M-'") 'conjecture-pprint-result)
    (define-key map (kbd "C-c '")   'conjecture-show-result)
    (define-key map (kbd "C-c k")   'conjecture-clear)
    (define-key map (kbd "C-c C-t") 'clojure-jump-between-tests-and-code)
    (define-key map (kbd "M-p")     'conjecture-previous-problem)
    (define-key map (kbd "M-n")     'conjecture-next-problem)
    map)
  "Keymap for Clojure test mode.")

;;;###autoload
(define-minor-mode conjecture-mode
  "A minor mode for running Clojure tests.

\\{conjecture-mode-map}"
  nil " Conjecture" conjecture-mode-map
  (when (conjecture-nrepl-connected-p)
    (conjecture-load-reporting)))

(add-hook 'nrepl-connected-hook 'conjecture-load-reporting)

;;;###autoload
(progn
  (defun conjecture-maybe-enable ()
    "Enable conjecture-mode if the current buffer contains a namespace
with a \"test.\" bit on it."
    (let ((ns (clojure-find-package))) ; defined in clojure-mode.el
      (when (and ns (string-match "test\\(\\.\\|$\\)" ns))
        (save-window-excursion
          (conjecture-mode t)))))

  (add-hook 'clojure-mode-hook 'conjecture-maybe-enable))

(provide 'conjecture-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; conjecture-mode.el ends here
