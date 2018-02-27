(provide 'my-dante)
;;(require 'my-links) 

(require 'haskell-mode)
(require 'dante) 

;; Company completion				| company-mode		| 
;; Type of selection				| dante-type-at		| C-c .
;; Info at point				| dante-info		| C-c ,
;; Apply Ghc suggestion for error at point	| dante-auto-fix	| C-c /
;; Goto definition				| xref-find-definitions	| M-.
;; Find uses					| xref-find-references	| M-?
;; REPLoid (*)					| dante-eval-block	| C-c ”
;; Restart                                      | dante-restart         | 
;; Diagnossis                                   | dante-diagnose        | 

;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Customization
;; `dante-project-root`
;; `dante-repl-command-line`
;; `dante-load-flags`
;;
;; to make sure that GHCi is properly loaded by dante
;; run `M-x customize-group dante` to read the documentation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GHCID 

(setq ghcid-filename   10)
(setq ghcid-line       20)
(setq ghcid-column     30)

   ;;NOTE -ferror-spans only
   ;; which cabal new-repl lacks
   ;;       10 (cons 20 21) (cons 30 31)
;; (setq ghcid-line-range       (20 . 21))
;; (setq ghcid-column-range     (30 . 31))

;;
(setq compilation-warning-type    0)
(setq compilation-info-type       1)
(setq compilation-error-type      2)

(setq ghcid-type       compilation-error-type)
(setq ghcid-hyperlink  nil) ;; `nil` means: match whole line
;(setq ghcid-highlight  )

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
;; TYPE is 2 or nil for a real error or 1 for warning or 0 for info.
;; TYPE can also be of the form (WARNING . INFO).  In that case this
;; will be equivalent to 1 if the WARNING’th subexpression matched
;; or else equivalent to 0 if the INFO’th subexpression matched.
;; See ‘compilation-error-face’, ‘compilation-warning-face’,
;; ‘compilation-info-face’ and ‘compilation-skip-threshold’.

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

;;NOTE for this regex, `ghcid` needs `--ghci-options -ferror-spans`
;;(defvar
(setq ghcid-error-regular-expression 
  (rx 

   line-start
;;TODO?   line-start 

   (group-n 10 ;;TODO ghcid-filename
            (one-or-more (not (any ":\n")))
	    ".hs") ;;TODO other extensions, like: .lhs, .hsc, 
   ":"
   (group-n 20  ;;TODO ghcid-line
	    (one-or-more digit))

   ;;NOTE -ferror-spans only
   ;; which cabal new-repl lacks
   ;; (group-n 21
   ;;          (zero-or-one "-" (one-or-more digit))) 

   ":"
   (group-n 30  ;;TODO ghcid-column
	    (one-or-more digit))

   ;;NOTE -ferror-spans only
   ;; which cabal new-repl lacks
   ;; (group-n 31
   ;;          (zero-or-one "-" (one-or-more digit))) 

   ": " 
   (or (group-n 42 "error")    ;;TODO 
       (group-n 41 "warning"))  ;;TODO 
   ":"
   (zero-or-more (not (any "\n")))

   line-end

   ;; (zero-or-more (not (any "\n")))
   ;; "error" 
   ;; (zero-or-more (not (any "\n")))

;;TODO?    line-end
)) 

;;
(setq ghcid-warning-regular-expression
  (rx 
   line-start
   (group-n 10 ;;TODO ghcid-filename
            (one-or-more (not (any ":\n")))
	    (or ".hs" ".lhs" ".hsc"))
   ":"
   (group-n 20  ;;TODO ghcid-line
	    (one-or-more digit))
   ":"
   (group-n 30  ;;TODO ghcid-column
	    (one-or-more digit))
   ": warning:"
   (zero-or-more (not (any "\n")))
   line-end)) 

(add-to-list 'compilation-error-regexp-alist       
 'ghcid-error)

(add-to-list 'compilation-error-regexp-alist       
 'ghcid-warning)

(add-to-list 'compilation-error-regexp-alist-alist
 (list 'ghcid-error
       ghcid-error-regular-expression
       ghcid-filename
       ghcid-line
       ghcid-column
       compilation-error-type
       ghcid-hyperlink
 )
) ;; (list 1 'compilation-error-face)))

(add-to-list 'compilation-error-regexp-alist-alist
 (list 'ghcid-warning
       ghcid-warning-regular-expression
       ghcid-filename
       ghcid-line
       ghcid-column
       compilation-warning-type
       ghcid-hyperlink))

;; defvar
;; (setq ghcid-file-regular-expression 
;;   (rx 
;;  ;;  string-start 
;;    (zero-or-more anything)
;;    "ghcid.txt"
;;    (zero-or-more anything)
;; ;;   string-end
;;   )) 

(setq ghcid-file-regular-expression ".*ghcid\\.txt\\'")

(add-to-list 'auto-mode-alist
 (cons ghcid-file-regular-expression 'ghcid-mode))

(defun ghcid-mode ()
  (interactive) 
  (toggle-read-only)
  (setq auto-revert-interval 1)
  (auto-revert-mode)
  (compilation-minor-mode))

;; "^\\(?10:[^:]+\\.hs\\):\\(?20:[[:digit:]]+\\):\\(?30:[[:digit:]]+\\): error:$"
;; ^\(?10:[^:]+\.hs\):\(?20:[[:digit:]]+\):\(?30:[[:digit:]]+\): error:$
;; ^[^:]+\.hs:[[:digit:]]+:[[:digit:]]+: error:$

(defun find-ghcid-txt ()
 "open/activate the nearest `ghcid.txt`, if it exists. works within multi-package projects too. TODO .hs extensions only?"
 (interactive)
 (find-file-in-ancestor-directory "ghcid.txt")
 (beginning-of-buffer)
 (revert-buffer t t)
 (setq unread-command-events (listify-key-sequence "\C-m"))
 ;; (setq unread-command-events (kbd "RET"))
)
 ;; (revert-buffer &optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
 ;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO workflow-windows sends numpad arrow keys instead of the normal ones 
;; (global-set-key (kbd "<kp-KEY>") 'COMMAND)
(global-set-key (kbd "<kp-home>")	'dante-type-at)		; C-c .
(global-set-key (kbd "<kp-end>")	'dante-info)		; C-c ,

(global-set-key (kbd "<kp-delete>")     	'find-ghcid-txt)

;;(global-set-key (kbd "<kp-space>")     	'xref-find-definitions)	; M-.

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
;; TODO (global-set-key (kbd "<kp-left>")	'pop-global-mark)
;; (global-set-key (kbd "<kp-right>")	')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
