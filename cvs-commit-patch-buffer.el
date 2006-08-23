;; Copyright 2003 Jim Radford and David Caldwell

(defun cvs-commit-patch-buffer (buffer directory)
  "Still need to get the list of modified files and tell them to revert"
  (interactive "bBuffer to commit: \nDDirectory: ")
  (log-edit `(lambda () (interactive)
			   (let ((file (make-temp-file "commit-buffer" nil))
					 (comment (buffer-string)) status
					 (cvs-commit-buffer (window-buffer (display-buffer (get-buffer-create "*cvs-commit-patch*")))))
				 (with-current-buffer cvs-commit-buffer (erase-buffer))
				 (unwind-protect
					 (progn
					   (with-current-buffer ,buffer
						 (write-region (point-min) (point-max) file))
					   (with-current-buffer cvs-commit-buffer
						 (let ((default-directory ,directory))
						   (unless (eq (setq status (call-process "commit-patch" nil cvs-commit-buffer 'display "-m" comment file)) 0)
							   (message "Commit patch failed with a status of '%S'." status))))
					   (delete-file file)))))
			nil nil "*cvs-commit*"))

(when (require 'diff-mode)
  (setq diff-default-read-only nil)
  (define-key diff-mode-map "\C-c\C-c" 'cvs-commit-patch-buffer))

(provide 'cvs-commit-patch-buffer)
