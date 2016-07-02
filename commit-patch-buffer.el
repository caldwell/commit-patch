;;; commit-patch-buffer.el --- commit patches to Darcs, Git, Mercurial, Bazaar, Monotone, Subversion, or CVS repositories

;; Copyright 2003-2015 Jim Radford <radford@bleackbean.org>
;;                 and David Caldwell <david@porkrind.org>
;; This code can be distributed under the terms of the GNU Public License (Version 2 or greater).
;;
;; Version: 2.5
;;
;; Author: Jim Radford <radford@blackbean.org>
;;         David Caldwell <david@porkrind.org>

;;; Commentary:

;; commit-patch-buffer provides an Emacs front end to the commit-patch(1)
;; program. Typically the patch to commit would be obtained with vc-diff
;; ("C-c v ="), though any Emacs diff-mode buffer can be committed.
;;
;; Typing "C-c C-c" in a diff-mode buffer kicks off the process and brings
;; up a buffer for the commit comment. After entering a suitable comment,
;; type "C-c C-c" again to finish the commit. If commit-patch-buffer cannot
;; automatically detect the repository directory, it will ask for it
;; interactively.
;;
;; commit-patch-buffer-in-directory is also available: this function skips
;; the automagical repository detection logic if the user wants to directly
;; specify the buffer to commit and directory.

;;; Code:

(require 'vc)
(require 'log-edit)


(defcustom commit-patch-program
  ;; Prefer a locally installed commit patch over one installed in the PATH.
  (let* ((this-path (or load-file-name (buffer-file-name)))
         (local-path (expand-file-name "commit-patch" (file-name-directory this-path))))
    (if (file-executable-p local-path) local-path "commit-patch"))
  "The pathname of the commit-patch executable to use. This could be a
string, or a function. If a function, then this is called every
time the program path is needed to retrieve the path. The
function takes no arguments, but can use variables such as
`default-directory'. This is useful to find commit-patch in different
places on different machines when using TRAMP."
  :type '(choice
	   (string :tag "Program path")
	   (function :tag "Function that returns the program path"))
  :group 'commit-patch)

(defun commit-patch--get-program ()
  "Retrieves the path to the commit-patch program. This is the value of
the `commit-patch-program' variable, if it is a string, or the value
returned by the `commit-patch-program' function, if it is a function."
  (let ((program
         (cond
          ((stringp   commit-patch-program) commit-patch-program)
          ((functionp commit-patch-program) (funcall commit-patch-program))
          (t (error "commit-patch-program must be a string or function: %s"
                    commit-patch-program)))))
    (or program (error "commit-patch-program is nil!"))))

;; Based on vc-git-expanded-log-entry, but don't indent and only grab the full comment using --pretty
(defun commit-patch-git-log-comment (revision)
  (with-temp-buffer
    (apply 'vc-git-command t nil nil (list "log" "--pretty=format:%B" revision "-1"))
    (goto-char (point-min))
    (unless (eobp)
      (buffer-string))))

;; Currently commit-patch only supports --amend with Git and Darcs.
;; But Darcs amend is very interactive, so we don't support it here.
(defun commit-patch-last-log-comment (directory)
  (let ((default-directory directory))
    (pcase (vc-responsible-backend directory)
      (`Git
       (commit-patch-git-log-comment "HEAD")))))


(defun commit-patch-buffer-in-directory (buffer directory &optional amend)
  "Commit the patch found in BUFFER by applying it to the
repository in DIRECTORY with commit-patch(1).  If AMEND is
non-nil, we amend the previous commit instead of creating a new
one."
  (interactive "bBuffer to commit: \nDDirectory: \nP")
  (let* ((patch-files (with-temp-buffer
                        (let ((lsdiff (current-buffer)))
                          (when (eq 0 (with-current-buffer buffer
                                        (call-process-region (point-min) (point-max)
                                                             "lsdiff" nil lsdiff nil)))
                            (split-string (buffer-string))))))
         (log-buffer-name (if amend "*amend*" "*commit*"))
         (f patch-files) visiting-buffers)
    (while (car f)
      (let ((buf (find-buffer-visiting (car f))))
        (when buf
          (with-current-buffer buf (vc-buffer-sync))
          (add-to-list 'visiting-buffers buf)))
      (setq f (cdr f)))
    (if amend
        (with-current-buffer (get-buffer-create log-buffer-name)
          (erase-buffer)
          (insert (or (commit-patch-last-log-comment directory) ""))
          (goto-char 0)))
    (log-edit
     `(lambda () (interactive)
        (let ((patch (make-temp-file "commit-buffer" nil))
              (comment (buffer-string))
              (output-buffer (get-buffer-create "*commit-patch*")))
          (unwind-protect
              (progn
                (with-current-buffer ,buffer
                  (write-region (point-min) (point-max) patch))
                (with-current-buffer output-buffer
                  (erase-buffer)
                  (let* ((default-directory ,directory)
                         (status (apply 'process-file (commit-patch--get-program) patch
                                               output-buffer nil
                                               (append `("-m" ,comment)
                                                     (if ,amend '("--amend"))))))
                    (if (not (eq status 0))
                        (progn
                          (window-buffer (display-buffer output-buffer))
                          (message "Commit patch failed with a status of '%S' (%S)." status patch))
                      (mapc (lambda (buf) (with-current-buffer buf
                                            (vc-resynch-buffer (buffer-file-name buf) 'revert 'noquery)
                                            ;; stupid vc-revert-buffer1 doesn't call revert-buffer
                                            ;; with preserve-modes which means the CVS version doesn't
                                            ;; get updated, so we do it by hand.
                                            (run-hooks 'find-file-hooks)))
                            ',visiting-buffers)
                      (message "Patched and %s %S file(s) and reverted %S."
                               (if ,amend "amended" "committed")
                               ,(length patch-files) ,(length visiting-buffers))))))
            (delete-file patch))))
     nil
     `((log-edit-listfun . (lambda () ',patch-files)))
     log-buffer-name)))

(defun commit-patch-buffer (&optional arg amend)
  "Commit the patch in the current buffer, applying it to the
repository in the appropriate directory with commit-patch(1). If
the current buffer is not in diff-mode or ARG is non-nil then it
will ask interactively which buffer to commit and to which
directory to commit it.  If AMEND is non-nil, we amend the
previous commit instead of creating a new one."
  (interactive "P")
  (if (and (not arg) (eq major-mode 'diff-mode))
      (commit-patch-buffer-in-directory (buffer-name) (autodetect-patch-directory-root) amend)

    (let ((current-prefix-arg amend))
      (call-interactively 'commit-patch-buffer-in-directory))))

(defun commit-patch-buffer-amend (&optional arg)
  "Commit the patch in the current buffer, applying it to the
repository in the appropriate directory with commit-patch(1). If
the current buffer is not in diff-mode or ARG is non-nil then it
will ask interactively which buffer to commit and to which
directory to commit it.  This is identical to
`commit-patch-buffer' except it amends the last commit by default
instead of creating a new one."
  (interactive "P")
  (commit-patch-buffer arg t))

(defun autodetect-patch-directory-root ()
  "Tries to autodect where a patch should be committed from using the
following algorithm:

   1. Grab the path mentioned in the first diff hunk of the
      current buffer and its buffer's full path.

   2. Strip common files/directories from end of paths.

   3. Return whatever is left over of buffer's path."
  (save-excursion
    (beginning-of-buffer)
    (diff-hunk-next) ;; Have to be in a hunk or diff-hunk-file-names won't work.
    (let ((diff-path (reverse (split-string (car (diff-hunk-file-names)) "/")))
          (file-path (reverse (split-string (buffer-file-name (car (diff-find-source-location))) "/"))))
      (while (string-equal (car file-path) (car diff-path))
        (setq file-path (cdr file-path))
        (setq diff-path (cdr diff-path)))
      ;; The extra "" here adds a / onto the end of our directory. Otherwise call-process (via process-file)
      ;; calls unhandled-file-name-directory which strips the last part of the path off if it doesn't end with
      ;; a /. Yes, this took multiple hours to figure out.
      (combine-and-quote-strings (reverse (cons "" file-path)) "/"))))

(eval-after-load 'diff-mode '(progn
  (setq diff-default-read-only nil)
  (define-key diff-mode-map "\C-c\C-c" 'commit-patch-buffer)
  (define-key diff-mode-map "\C-xvv" 'commit-patch-buffer)
  (define-key diff-mode-map (kbd "C-c C-S-c") 'commit-patch-buffer-amend)))

(provide 'commit-patch-buffer)

;;; commit-patch-buffer.el ends here
