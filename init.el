;; M-: user-init-file
;; c:/Users/Spiros/AppData/Roaming/.emacs.d/init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; '''The way I use to maintain several .emacs.d directories in parallel is the following.

;; emacs is started like this:

;; alias emacs-windows='./result/bin/emacs -q --load "~/.emacs.d-windows/init.el"'
;; alias emacs-here='./result/bin/emacs -q --load "./init.el"' # relative filepath

;; Each init.el file begins like this, to correctly set up the user-init-file and user-emacs-directory variables:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; The patch which allows you to specify .emacs.d location via `EMACS_USER_DIRECTORY' environment variable is available but not merged.'''

(defvar config user-emacs-directory)
;; (defvar config "c:/Users/Spiros/AppData/Roaming/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package) 

(defun add-package-repository (repository) 
  (add-to-list 'package-archives repository t))

(add-package-repository '("gnu"          . "http://elpa.gnu.org/packages/")) 
(add-package-repository '("melpa"        . "https://melpa.org/packages/"))
(add-package-repository '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (eval-when-compile
;;   (require 'use-package))
(require 'use-package)

(add-to-list 'load-path (concat config "packages/"))
(add-to-list 'load-path (concat config "my/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SAVING
;;must come first 

(require 'real-auto-save)
(add-hook 'fundamental-mode 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'text-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1) ;; in seconds

(setq auto-save-visited-file-name t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-save-mode 1)
(setq desktop-auto-save-timeout 5) ;; in seconds 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-file user-init-file) ;; (concat config "init.el"))
(find-file (concat config "notes.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions 

(defun maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(global-set-key (kbd "<tab>") 'dabbrev-expand)
;; (global-set-key (kbd "<f1>")  'execute-extended-command) ;; stupid Dragon NaturallySpeaking shortcut, it crashes when I edit the shortcuts, I should try to find any entries in the registry
(global-set-key (kbd "<f2>")  'eval-expression)
(global-set-key (kbd "<f11>") 'pp-eval-expression) ;; eval-expression
(global-set-key (kbd "<f12>") 'execute-extended-command)
;; (global-set-key (kbd "<kp-insert>") 'execute-extended-command)

(global-set-key (kbd "<kp-insert>") 'electric-buffer-list) 

(global-set-key (kbd "<pause>") 'set-mark-command)
(global-set-key "\M-r" 'query-replace-regexp)
(global-set-key "\M-`" 'previous-buffer) ;; mnemonic: it's near ALT-TAB 

;; NOTE Windows-specific 
(global-set-key (kbd "<apps>") 'execute-extended-command)

(global-set-key (kbd "C-M-m") 'maximize-frame) 

; (global-set-key (kbd "<f9>") 'pop-tag-mark)
					;(global-set-key (kbd "<f10>") 'intero-goto-definition)

;; (global-set-key (kbd "<kp-home>") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFERS 

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)
(add-hook 'Buffer-menu-mode-hook (lambda() 
;;  (setq Buffer-menu-files-only t) 
  (revert-buffer)))
  ;; see buff-menu.el 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)

;; Prevent Extraneous Tabs
;; Note that this line uses setq-default rather than the setq command that we have seen before. The setq-default command sets values only in buffers that do not have their own local values for the variable.
(setq-default indent-tabs-mode nil)

;; disable automatic indentation on newlines 
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;;(add-hook 'after-init-hook 'global-company-mode)

(prefer-coding-system 'utf-8)

(setq
 redisplay-dont-pause t
;;  scroll-margin 10
 scroll-step 1
;;  scroll-conservatively 10000
 scroll-preserve-screen-position 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

(set-background-color "#f4f4f4")
;; https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;; hide menubar and toolbar
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)

;; peek
;; (setq
;;  redisplay-dont-pause t
;;  scroll-margin 10
;;  scroll-step 1
;;  scroll-conservatively 10000
;;  scroll-preserve-screen-position 1)

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAMES AND WINDOWS 


;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html#Window-Frame-Parameters

;; (set-frame-parameter nil ' ')

(set-frame-parameter nil 'title "EMACS") 
;; the title of the operating-system-window 

(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;; (set-frame-parameter nil 'border-width 0)
;; ERRORS with "error: Cannot change the border width of a frame" 
;; The width (in pixels) of the frame's border.

(set-frame-parameter nil 'left-fringe 0)
(set-frame-parameter nil 'right-fringe nil)
;; an integer, or nil (for the default) 

;; (set-frame-parameter nil 'tool-bar-position 'bottom)
;; top, bottom, left, or right
;; only works on GTK 

;; unsplittable
;; If non-nil, this frame's window is never split automatically.

;; icon-type
;; The type of icon to use for this frame. If the value is a string, that specifies a file containing a bitmap to use; nil specifies no icon (in which case the window manager decides what to show); any other non-nil value specifies the default Emacs icon.

;; window-id
;; The ID number which the graphical display uses for this frame. Emacs assigns this parameter when the frame is created; changing the parameter has no effect on the actual ID number.

;; sticky
;; If non-nil, the frame is visible on all virtual desktops on systems with virtual desktops.

;; font-backend
;; A list of symbols, specifying the font backends to use for drawing fonts in the frame, in order of priority. On X, there are currently two available font backends: x (the X core font driver) and xft (the Xft font driver). On MS-Windows, there are currently two available font backends: gdi and uniscribe (see Windows Fonts).

;; screen-gamma
;; If this is a number, Emacs performs gamma correction which adjusts the brightness of all colors. The value should be the screen gamma of your display.
;; Usual PC monitors have a screen gamma of 2.2, so color values in Emacs, and in X windows generally, are calibrated to display properly on a monitor with that gamma value.
;; If your monitor displays colors too light, you should specify a screen-gamma value smaller than 2.2. This requests correction that makes colors darker. A screen gamma value of 1.5 may give good results for LCD color displays.

;; alpha
;; This parameter specifies the opacity of the frame, on graphical displays that support variable opacity. It should be an integer between 0 and 100, where 0 means completely transparent and 100 means completely opaque. 

(set-frame-parameter nil 'vertical-scroll-bars 'right)
;; left, right, or nil (for no scroll bars) 
;; there is no "outer" option 
;; "left" is more convenient for scrolling, but it's too sensitive and causes misclicks when I'm trying to click at the start of a line in the leftmost window. thus, "right".  
;;  to “undo” (and “redo”) changes in the window configuration with the key commands ‘C-c left’ and ‘C-c right’
(when (fboundp 'winner-mode)
      (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs.stackexchange.com/questions/7475/recursively-go-up-to-find-makefile-and-compile

(defun find-file-in-ancestor-directory (FILE)
  "Traveling backwards in the filesystem from the current file's directory, find FILE and open (or activate) it in another window. (TODO returning the buffer."
  (interactive)
  (let* ((directory (locate-dominating-file default-directory FILE))
         (filepath  (concat directory FILE)))
    (progn
      (if directory
       (find-file-other-window filepath)
       (message "[find-file-in-ancestor-directory] not found: `%s`" FILE)))))

;; 
;;
;; https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
(defun press-C-g ()
  (interactive)
  (setq unread-command-events (listify-key-sequence "\C-g")))

(require 'my-dante)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq smerge-command-prefix "\C-cv")

;; TODO 
(defun my-haskell-windows ()
  "a configuration of Windows. the source (.hs) on the left, and the errors (ghcid) on the right."
  (interactive)
  (delete-other-windows)

  (find-file "*.cabal")

  (split-window-vertically) 

  ;; The last line insert this “current” configuration in a register, after changing the window buffers or the window configuration itself, you can return to this window configuration with { C-x r j w }.
  (window-configuration-to-register ?w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VOICE

(defun refresh-mode ()
  (interactive) 
  (toggle-read-only)
  (setq auto-revert-interval 1)
  (auto-revert-mode))

(setq shared-folder-transcription-file-regular-expression "transcription\\.txt\\'")

(add-to-list 'auto-mode-alist (cons shared-folder-transcription-file-regular-expression 'refresh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS  

;; see http://ivanmalison.github.io/dotfiles/

(defmacro make-interactive-function (function)
  `(lambda (&rest args)
     (interactive)
     (apply ,function args)))

(defmacro measure-time-of (&rest body)
  "Measure the running time of the given code block, returning the result."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun get-last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
    (forward-line (- 1 num))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))))

(defun random-choice-from (choices)
  (nth (random (length choices)) choices))

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'color-theme)

;; M-: (face-attribute 'default :background)
;; "#2e3436"

(defun darkroom-mode ()
	"Make things simple-looking by removing decoration 
	 and choosing a simple theme."
        (interactive)
;        (switch-full-screen 1)     ;; requires above function 
;;	(color-theme-retro-green)  ;; requires color-theme
;        (setq left-margin 10)
;        (menu-bar-mode -1)
;        (tool-bar-mode -1)
;        (scroll-bar-mode -1)

;        (set-face-foreground 'mode-line "gray15")
;        (set-face-background 'mode-line "black")
        (set-face-background 'default   "#2e3436")
        (auto-fill-mode 1)
)

(defun darkroom-mode-reset ()
   (interactive)
;   (switch-full-screen -1)
;   (color-theme-subtle-hacker) ;; Choose your favorite theme
;   (menu-bar-mode 1)
;   (tool-bar-mode 1)
;   (scroll-bar-mode 1)
;   (set-left-margin 0)
)	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SHORTCUTS (this is later to be defined after its dependent definitions)

(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFECTS

(darkroom-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package)))
 '(safe-local-variable-values
   (quote
    ((dante-repl-command-line "nix-shell" "/home/sboo/haskell/cards/default.nix" "-A" "shells.ghc" "--run" "cabal new-repl cards-frontend")
     (dante-repl-command-line "nix-shell" "/home/sboo/haskell/magic-card-search/shell.nix" "--run" "cabal repl magic-card-search")))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
