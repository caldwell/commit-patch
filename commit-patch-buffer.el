;; Copyright 2003-2010 Jim Radford <radford@bleackbean.org>
;;                 and David Caldwell <david@porkrind.org>, All Rights Reserved.
;; This code can be distributed under the terms of the GNU Public License
;; Version: 2.1

(require 'vc)
(require 'log-edit)

(defun commit-patch-buffer-manually (buffer directory)
  "Commit the patch found in BUFFER applying it from DIRECTORY."
  (interactive "bBuffer to commit: \nDDirectory: ")
  (let* ((patch-files (with-temp-buffer
                        (let ((lsdiff (current-buffer)))
                          (when (eq 0 (with-current-buffer buffer
                                        (call-process-region (point-min) (point-max) 
                                                             "lsdiff" nil lsdiff nil)))
                            (split-string (buffer-string)))))) 
         (f patch-files) visiting-buffers)
    (while (car f)
      (let ((buf (find-buffer-visiting (car f))))
        (when buf
          (with-current-buffer buf (vc-buffer-sync))
          (add-to-list 'visiting-buffers buf)))
      (setq f (cdr f)))
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
                         (status (process-file "commit-patch" patch
                                               output-buffer 'display
                                               "-m" comment)))
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
                      (message "Patched and commited %S file(s) and reverted %S." 
                               ,(length patch-files) ,(length visiting-buffers))))))
            (delete-file patch))))
     nil
     `((log-edit-listfun . (lambda () ',patch-files)))
     "*commit*")))

(defun commit-patch-buffer ()
  "Commit the patch in the current buffer, applying it to the appropriate directory."
  (interactive)
  (commit-patch-buffer-manually (buffer-name) (autodetect-patch-directory-root)))

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
  (define-key diff-mode-map "\C-xvv" 'commit-patch-buffer)))

(provide 'commit-patch-buffer)
