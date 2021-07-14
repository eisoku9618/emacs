;; trivial setting
(progn
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  ;; (tool-bar-mode 0)
  (menu-bar-mode -1)
  (progn
    (setq visible-bell t)
    (setq ring-bell-function 'ignore))
  (progn
    (setq scroll-conservatively 1);C-n C-p
    (setq next-screen-context-lines 0);C-v M-v
    )
  (progn
    (setq backup-inhibited t);do not create backup files
    (setq delete-auto-save-files t);delete auto save files when emacs is closed
    )
  (progn
    (show-paren-mode 1);hilight a set of ()
    (setq show-paren-delay 0) ;the delay until a set of () is highlighted
    )
  (setq-default indent-tabs-mode nil);use space instead of tab
  (setq-default transient-mark-mode t);hilight a selection
  (setq-default show-trailing-whitespace t)
  (global-auto-revert-mode 1)           ;See http://maruta.be/emacs/12
  (setq-default bidi-display-reordering nil
                bidi-paragraph-direction (quote left-to-right))
  (progn                                ;cua mode enable
    (cua-mode t)
    (setq cua-enable-cua-keys nil))

  (load-theme 'wheatgrass)
  )


;; about elisp
(progn
  ;; See http://d.hatena.ne.jp/m-hiyama/20081128/1227855376
  (when (require 'package nil t)
    (defvar package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (package-initialize)                ;call this function before use elpa el such as tabbar
    (defvar my/favorite-packages
      '(
        auto-async-byte-compile
        auto-complete
        helm
        helm-descbinds
        markdown-mode
        multi-term
        popup
        popwin
        tabbar
        wgrep
        yaml-mode
        cmake-mode
        ))
    (let (f)
      (dolist (package my/favorite-packages)
        (unless (package-installed-p package)
	  (if (null f)
	      (progn
		(package-refresh-contents)
		(setq f t)))
	  (package-install package))))
    )

  (when (require 'tabbar nil t)
    (tabbar-mode)
    (setq tabbar-buffer-list-function
          #'(lambda ()
              (delq nil
                    (mapcar #'(lambda (b)
                                (cond
                                 ((eq (current-buffer) b) b)
                                 ((buffer-file-name b) b)
                                 ((char-equal ?\ (aref (buffer-name b) 0)) nil)
                                 ((equal "*scratch*" (buffer-name b)) b)
                                 ((string-match "*shell" (buffer-name b)) b)
                                 ((string-match "*terminal" (buffer-name b)) b)
                                 ((char-equal ?* (aref (buffer-name b) 0)) nil)
                                 ((buffer-live-p b) b)))
                            (buffer-list)))))
    ;; (global-set-key [(C M f)] 'tabbar-forward)
    ;; (global-set-key [(C M b)] 'tabbar-backward)
    (global-set-key [(M right)] 'tabbar-forward)
    (global-set-key [(M left)] 'tabbar-backward)
    (tabbar-mwheel-mode nil)
    (setq tabbar-buffer-groups-function nil)
    (setq tabbar-separator '(1.5))
    (dolist (btn '(tabbar-buffer-home-button tabbar-scroll-left-button tabbar-scroll-right-button))
      (set btn (cons (cons "" nil)
                     (cons "" nil))))
    )

  ;; helm
  (when (require 'helm-descbinds nil t) (helm-descbinds-mode))
  ;; (when (require 'auto-complete nil t)
  ;;   (global-auto-complete-mode t)
  ;;   ;; (setq ac-modes (cons 'lisp-mode ac-modes))
  ;;   )

  ;; Load the library and start it up
  (if (getenv "ROS_DISTRO")
      (progn
        (add-to-list 'load-path (format "/opt/ros/%s/share/emacs/site-lisp" (getenv "ROS_DISTRO")))
        (require 'rosemacs-config nil t)))

  (when (require 'auto-async-byte-compile nil t)
                                        ;(setq auto-async-byte-compile-exclude-files-regexp "init.el")
    (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

  (when (require 'uniquify nil t) (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
  (when (require 'multi-term nil t) (setq multi-term-program "/bin/bash"))
  )

;; about mode
(progn
  ;; example : *hoge* +hoge+
  (font-lock-add-keywords
   'lisp-mode (list
               (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
               (list "\\(\\+\\w\+\\+\\)\\>" '(1 font-lock-constant-face nil t))))

  ;; an AssociationList that associates MajorModes with a pattern to match a buffer filename when it is first opened
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.ino" . c++-mode))

  ;; shell mode
  (progn
    (set-terminal-coding-system 'utf-8)
    (set-buffer-file-coding-system 'utf-8)
    (setq explicit-shell-file-name shell-file-name)
    (setq shell-command-option "-c")
    (setq system-uses-terminfo nil)
    (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'shell-mode-hook #'(lambda () (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

  (when (locate-library "vrml-mode")
    (autoload 'vrml-mode "vrml-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.wrl\\'" . vrml-mode))
    )

  ;; (add-hook 'c-mode-common-hook
  ;;           #'(lambda ()
  ;;               (define-key c++-mode-map "/" 'self-insert-command)
  ;;               (setq comment-style 'extra-line)
  ;;               (setq comment-continue " * ")
  ;;               (setq comment-start "/* ")
  ;;               (setq comment-end " */")))
  )


;;; Global Setting Key
;; See http://d.hatena.ne.jp/tomoya/20090415/1239809615
(progn
  (global-set-key [(C h)] 'backward-delete-char)
  (global-set-key [(M g)] 'goto-line)
  (global-set-key [(C x) (C b)] 'bs-show)
  ;; Timestamp
  ;; See http://stackoverflow.com/questions/1250846/wrong-type-argument-commandp-error-when-binding-a-lambda-to-a-key
  (global-set-key [(C c) (C d)] #'(lambda () (interactive) (insert (current-time-string))))
  (global-set-key [(M p)] #'(lambda (n) (interactive "p") (forward-line (* -1 n)) (scroll-down n)))
  (global-set-key [(M n)] #'(lambda (n) (interactive "p") (forward-line n) (scroll-up n)))
  (global-set-key [(C q)] #'(lambda (n) (interactive "p") (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1)) ((looking-at "\\s\)") (forward-char 1) (backward-list 1)))))
  (global-set-key [(C x) (b)] 'helm-for-files)
  ;; (global-set-key [(C x) (C f)] 'helm-find-files)
  (global-set-key [(M y)] 'helm-show-kill-ring)
  (global-set-key [(C o)] 'dabbrev-expand)
  )

;; fix emacs bag
(defadvice terminal-init-xterm (after fix-some-keys activate)
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[4~" [end]))

;; for *scratch*
(progn
  (defun my-make-scratch (&optional arg)
    (interactive)
    (progn
      (set-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (erase-buffer)
      (when (and initial-scratch-message (not inhibit-startup-message))
        (insert initial-scratch-message))
      (or arg (progn (setq arg 0)
                     (switch-to-buffer "*scratch*")))
      (cond ((= arg 0) (message "*scratch* is cleared up."))
            ((= arg 1) (message "another *scratch* is created")))))
  (add-hook 'kill-buffer-query-functions
            #'(lambda ()
                (if (string= "*scratch*" (buffer-name))
                    (progn (my-make-scratch 0) nil)
                  t)))
  (add-hook 'after-save-hook
            #'(lambda ()
                (unless (member (get-buffer "*scratch*") (buffer-list))
                  (my-make-scratch 1))))
  )

;; UTF-8 : put these lines at the end of init.el
(progn
  (prefer-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8))
