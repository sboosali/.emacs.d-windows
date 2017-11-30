(provide 'my-dante)
(require 'my-links) 
 
(require 'dante) 

;; Company completion				| company-mode		| 
;; Type of selection				| dante-type-at		| C-c .
;; Info at point				| dante-info		| C-c ,
;; Apply Ghc suggestion for error at point	| dante-auto-fix	| C-c /
;; Goto definition				| xref-find-definitions	| M-.
;; Find uses					| xref-find-references	| M-?
;; REPLoid (*)					| dante-eval-block	| C-c ”

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GHCID 

;; see https://www.emacswiki.org/emacs/AutoModeAlist
;; The auto-mode-alist variable is an AssociationList that associates MajorModes with a pattern to match a buffer filename when it is first opened
;; the string matched is the full pathname
;; { \\' } means "match the end of the string" ?? 
;; Backslashes must be entered as { \\. } ??  

(setq auto-revert-interval 1)

;; (rx bol
;;   (zero-or-more blank)
;;   (one-or-more digit)
;;   ":")
;; 
;; ->
;; 
;; "^[[:blank:]]*[[:digit:]]+:"

;; (rx 
;;   line-start 
;;   "C:\\" 
;;   (one-or-more (or alphanumeric (or blank "-" "_" ) (or "\\" "/"))) 
;;   ".hs"
;;   ":"
;;   (one-or-more digit))

;; (setq compilation-error-regexp-alist-alist ) 

;; see compilation-error-regexp-alist 
;; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...]).
;; If REGEXP matches, the FILE’th subexpression
;; gives the file name, and the LINE’th subexpression gives the line
;; number, the COLUMN’th subexpression gives the column number on
;; that line.
;; LINE can also be of the form (LINE . END-LINE) meaning a range
;; of lines.  COLUMN can also be of the form (COLUMN . END-COLUMN)
;; meaning a range of columns starting on LINE and ending on
;; END-LINE, if that matched.
;; What matched the HYPERLINK’th subexpression has ‘mouse-face’ and
;; ‘compilation-message-face’ applied.  If this is nil, the text
;; matched by the whole REGEXP becomes the hyperlink.

;; lookup the value of a key in an association list 
;; (cdr (assoc 'KEY ALIST))
;; 
;; (assoc 'java compilation-error-regexp-alist-alist) 
;; (java "^\\(?:[ 	]+at \\|==[0-9]+== +\\(?:at\\|b\\(y\\)\\)\\).+(\\([^()\n]+\\):\\([0-9]+\\))$" 2 3 nil (1))

;; example: 
;; 
;; (add-to-list 'compilation-error-regexp-alist-alist
;;      '(my-message-error
;;        "^\\(ERRR\\|CRIT\\|ALRT\\|EMRG\\) .*\\[\\(\\([^ \n]+\\):\\([0-9]+\\)\\)\\]$"
;;        3 4 nil nil 2
;;        (1 compilation-error-face)))

;; for this regex, `ghcid` needs `--ghci-options -ferror-spans`
;;(defvar
(setq ghcid-error-regular-expression 
  (rx 
   line-start 
   (group-n 10 
            (one-or-more (not (any ":\n")))
	    ".hs")
   ":"
   (group-n 20
	    (one-or-more digit))
   (group-n 21
	    (zero-or-one "-" (one-or-more digit))) 
   ":"
   (group-n 30
	    (one-or-more digit))
   (group-n 31
	    (zero-or-one "-" (one-or-more digit))) 
   (zero-or-more (not (any "\n")))
   "error" 
   (zero-or-more (not (any "\n")))
   line-end
   )) 

   ;; (group-n 5 
   ;; 	    ;; (or "C:\\" "/")
   ;;          "C:\\" 
   ;; 	    ;; (one-or-more (or alphanumeric (or blank "-" "_" ) (or "\\" "/"))) 
   ;;          (one-or-more (or alphanumeric blank "-" "_" "\\" "/"))
   ;; 	    ".hs")

(add-to-list 'compilation-error-regexp-alist       'ghcid-error)
(add-to-list 'compilation-error-regexp-alist-alist
 (list 'ghcid-error
       ghcid-error-regular-expression
       10 (cons 20 21) (cons 30 31) 
       nil 5)) ;; (list 1 'compilation-error-face)))

;; defvar
;; (setq ghcid-file-regular-expression 
;;   (rx 
;;  ;;  string-start 
;;    (zero-or-more anything)
;;    "ghcid.txt"
;;    (zero-or-more anything)
;; ;;   string-end
;;   )) 

(setq ghcid-file-regular-expression "ghcid\\.txt\\'")
(add-to-list 'auto-mode-alist (cons ghcid-file-regular-expression 'ghcid-mode))
(defun ghcid-mode ()
  (interactive) 
  (toggle-read-only)
  (setq auto-revert-interval 1)
  (auto-revert-mode)
  (compilation-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO workflow-windows sends numpad arrow keys instead of the normal ones 
;; (global-set-key (kbd "<kp-KEY>") 'COMMAND)
(global-set-key (kbd "<kp-home>")	'dante-type-at)		; C-c .
(global-set-key (kbd "<kp-end>")	'dante-info)		; C-c ,
(global-set-key (kbd "<kp-space>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-decimal>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-up>")	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-down>")	'xref-find-references)	; M-?
;; (global-set-key (kbd "<kp-prior>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-next>") 	'xref-find-references)	; M-?

;; (global-set-key (kbd "<kp-KEY>")	'dante-eval-block)	; C-c ”
;; (global-set-key (kbd "<kp-KEY>") 'dante-auto-fix) 		; C-c /

;; <kp-0>		0
;; <kp-1>		1
;; <kp-2>		2
;; <kp-3>		3
;; <kp-4>		4
;; <kp-5>		5
;; <kp-6>		6
;; <kp-7>		7
;; <kp-8>		8
;; <kp-9>		9
;; <kp-add>	+
;; <kp-begin>	<begin>
;; <kp-decimal>	.
;; <kp-delete>	<deletechar>
;; <kp-divide>	/
;; <kp-down>	<down>
;; <kp-end>	<end>
;; <kp-enter>	RET
;; <kp-equal>	=
;; <kp-home>	<home>
;; <kp-insert>	<insert>
;; <kp-left>	<left>
;; <kp-multiply>	*
;; <kp-next>	<next>
;; <kp-prior>	<prior>
;; <kp-right>	<right>
;; <kp-separator>	,
;; <kp-space>	SPC
;; <kp-subtract>	-
;; <kp-tab>	TAB
;; <kp-up>		<up>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARK RING 

(setq global-mark-ring-max 1000)
(global-set-key (kbd "<kp-left>")	'pop-global-mark)
;; (global-set-key (kbd "<kp-right>")	')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
