(defvar dash-mode-hook nil)

(defvar dash-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for dash major mode")

(add-to-list 'auto-mode-alist '("\\.skye\\'" . wpdl-mode))

(defconst dash-font-lock-keywords-1
  (list
   '("\\<fun\\|let\\|in\\|glet\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for dash mode")

(defvar dash-font-lock-keywords dash-font-lock-keywords-1
  "Highlighting for dash keywords")

(defun dash-mode ()
  "Major mode for dash's .skye files"
  (interactive)
  (kill-all-local-variables)
  ;(set-syntax-table wpdl-mode-syntax-table)
  (use-local-map dash-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dash-font-lock-keywords))
  (setq major-mode 'dash-mode)
  (setq mode-name "Dash/Skye")
  (run-hooks 'dash-mode-hook))

(provide 'dash-mode)

