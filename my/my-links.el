(provide 'my-links) 

(define-button-type 'find-file-button
  'follow-link t
  'action #'find-file-button)

(defun find-file-button (button)
  (find-file (buffer-substring (button-start button) (button-end button))))

(defun buttonize-buffer ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx ) nil t) ;; "/[^ \t]*"
      (make-button (match-beginning 0) (match-end 0) :type 'find-file-button))))

(add-hook 'text-mode-hook 'buttonize-buffer)
