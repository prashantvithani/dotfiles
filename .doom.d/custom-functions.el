;;; $DOOMDIR/custom-functions.el -*- lexical-binding: t; -*-

;; ----- Paste with indent -----
(evil-define-command evil-paste-before-and-indent
  (count &optional register yank-handler)
  "Pastes the latest yanked text before point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
  (interactive "P<x>")
  (evil-with-single-undo
    (let ((text (evil-paste-before count register yank-handler)))
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
      text)))

(evil-define-command evil-paste-after-and-indent
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
  (interactive "P<x>")
  (evil-with-single-undo
    (let ((text (evil-paste-after count register yank-handler)))
      (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
      text)))

;; ----- Treemacs (temporary fix for 6c431174dd933f87c32f283740ad2078e1c4fcb8) -----
(defun fix-treemacs ()
  (interactive)
  (setf treemacs--file-name-handler-alist
        (with-no-warnings
          (list
           (cons tramp-file-name-regexp #'tramp-file-name-handler)))))

;; ----- HideShow functions -----
;; not used
(defun +data-hideshow-forward-sexp (arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (require 'evil-indent-plus)
      (let ((range (evil-indent-plus--same-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))

;; ----- ORG Functions -----
(defun +org*update-cookies ()
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

;; ----- Custom User functions -----
(defun user/fill-column-indicator-hooks ()
  (add-hook 'ruby-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'c-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'java-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'python-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'go-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'common-lisp-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  (add-hook 'scheme-mode-hook (lambda () (display-fill-column-indicator-mode t)))
  )

(defun user/dictionary ()
  "All settings related to dictionary"
  ;;;; Dictionary
  (require 'dictionary)

  ;; Customize
  (setq dictionary-default-dictionary "wn")

  ;; key bindings - history go back
  (evil-define-key 'motion dictionary-mode-map
    (kbd "C-o") 'dictionary-previous)

  ;; Invoke
  ;; (spacemacs/set-leader-keys "xwf" 'dictionary-lookup-definition)
  ;; (spacemacs/set-leader-keys "xwD" 'dictionary-search)

  ;; Font face -- make dictionary beautiful
  (set-face-font 'dictionary-word-definition-face "Cascadia Code")
  (set-face-attribute 'dictionary-button-face nil :foreground "magenta")
  (set-face-attribute 'dictionary-reference-face nil :foreground "cyan")
  (set-face-attribute 'dictionary-word-entry-face nil :foreground "yellow")
  )

;; ---------------- TRAMP FIX FOR MAGIT HUNK STAGE ---------------
(defun tramp-sh-handle-make-process-override (orig-func &rest args)
  "Like `make-process' for Tramp files.
STDERR can also be a remote file name.  If method parameter
`tramp-direct-async' and connection property
\"direct-async-process\" are non-nil, an alternative
implementation will be used."
  (if (tramp-direct-async-process-p args)
      (apply #'tramp-handle-make-process args)
    (when args
      (with-parsed-tramp-file-name (expand-file-name default-directory) nil
        (let ((name (plist-get args :name))
              (buffer (plist-get args :buffer))
              (command (plist-get args :command))
              (coding (plist-get args :coding))
              (noquery (plist-get args :noquery))
              (connection-type
               (or (plist-get args :connection-type) process-connection-type))
              (filter (plist-get args :filter))
              (sentinel (plist-get args :sentinel))
              (stderr (plist-get args :stderr)))
          (unless (stringp name)
            (signal 'wrong-type-argument (list #'stringp name)))
          (unless (or (bufferp buffer) (string-or-null-p buffer))
            (signal 'wrong-type-argument (list #'bufferp buffer)))
          (unless (or (null command) (consp command))
            (signal 'wrong-type-argument (list #'consp command)))
          (unless (or (null coding)
                      (and (symbolp coding) (memq coding coding-system-list))
                      (and (consp coding)
                           (memq (car coding) coding-system-list)
                           (memq (cdr coding) coding-system-list)))
            (signal 'wrong-type-argument (list #'symbolp coding)))
          (when (eq connection-type t)
            (setq connection-type 'pty))
          (unless (or (and (consp connection-type)
                           (memq (car connection-type) '(nil pipe pty))
                           (memq (cdr connection-type) '(nil pipe pty)))
                      (memq connection-type '(nil pipe pty)))
            (signal 'wrong-type-argument (list #'symbolp connection-type)))
          (unless (or (null filter) (eq filter t) (functionp filter))
            (signal 'wrong-type-argument (list #'functionp filter)))
          (unless (or (null sentinel) (functionp sentinel))
            (signal 'wrong-type-argument (list #'functionp sentinel)))
          (unless (or (bufferp stderr) (string-or-null-p stderr))
            (signal 'wrong-type-argument (list #'bufferp stderr)))
          (when (and (stringp stderr)
                     (not (tramp-equal-remote default-directory stderr)))
            (signal 'file-error (list "Wrong stderr" stderr)))

          (let* ((buffer
                  (if buffer
                      (get-buffer-create buffer)
                    ;; BUFFER can be nil.  We use a temporary buffer.
                    (generate-new-buffer tramp-temp-buffer-name)))
                 ;; STDERR can also be a file name.
                 (tmpstderr
                  (and stderr
                       (tramp-unquote-file-local-name
                        (if (stringp stderr)
                            stderr (tramp-make-tramp-temp-name v)))))
                 (remote-tmpstderr
                  (and tmpstderr (tramp-make-tramp-file-name v tmpstderr)))
                 (orig-command command)
                 (program (car command))
                 (args (cdr command))
                 ;; When PROGRAM matches "*sh", and the first arg is
                 ;; "-c", it might be that the arguments exceed the
                 ;; command line length.  Therefore, we modify the
                 ;; command.
                 (heredoc (and (not (bufferp stderr))
                               (stringp program)
                               (string-match-p (rx "sh" eol) program)
                               (tramp-compat-length= args 2)
                               (string-equal "-c" (car args))
                               ;; Don't if there is a quoted string.
                               (not
                                (string-match-p (rx (any "'\"")) (cadr args)))
                               ;; Check, that /dev/tty is usable.
                               (tramp-get-remote-dev-tty v)))
                 ;; When PROGRAM is nil, we just provide a tty.
                 (args (if (not heredoc) args
                         (let ((i 250))
                           (while (and (not (tramp-compat-length< (cadr args) i))
                                       (string-match " " (cadr args) i))
                             (setcdr
                              args
                              (list
                               (replace-match " \\\\\n" nil nil (cadr args))))
                             (setq i (+ i 250))))
                         (cdr args)))
                 ;; Use a human-friendly prompt, for example for
                 ;; `shell'.  We discard hops, if existing, that's why
                 ;; we cannot use `file-remote-p'.
                 (prompt (format "PS1=%s %s"
                                 (tramp-make-tramp-file-name v)
                                 tramp-initial-end-of-output))
                 ;; We use as environment the difference to toplevel
                 ;; `process-environment'.
                 env uenv
                 (env (dolist (elt (cons prompt process-environment) env)
                        (or (member
                             elt (default-toplevel-value 'process-environment))
                            (if (tramp-compat-string-search "=" elt)
                                (setq env (append env `(,elt)))
                              (setq uenv (cons elt uenv))))))
                 (env (setenv-internal
                       env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
                 (command
                  (when (stringp program)
                    (format "cd %s && %s exec %s %s env %s %s"
                            (tramp-shell-quote-argument localname)
                            (if uenv
                                (format
                                 "unset %s &&"
                                 (mapconcat
                                  #'tramp-shell-quote-argument uenv " "))
                              "")
                            (if heredoc
                                (format "<<'%s'" tramp-end-of-heredoc) "")
                            (if tmpstderr (format "2>'%s'" tmpstderr) "")
                            (mapconcat #'tramp-shell-quote-argument env " ")
                            (if heredoc
                                (format "%s\n(\n%s\n) </dev/tty\n%s"
                                        program (car args) tramp-end-of-heredoc)
                              (mapconcat #'tramp-shell-quote-argument
                                         (cons program args) " ")))))
                 (tramp-process-connection-type
                  (or (null program) tramp-process-connection-type))
                 (bmp (and (buffer-live-p buffer) (buffer-modified-p buffer)))
                 (name1 name)
                 (i 0)
                 ;; We do not want to raise an error when
                 ;; `make-process' has been started several times in
                 ;; `eshell' and friends.
                 tramp-current-connection
                 p)

            ;; Handle error buffer.
            (when (bufferp stderr)
              (unless (tramp-get-remote-mknod-or-mkfifo v)
                (tramp-error
                 v 'file-error "Stderr buffer `%s' not supported" stderr))
              (with-current-buffer stderr
                (setq buffer-read-only nil))
              (tramp-taint-remote-process-buffer stderr)
              ;; Create named pipe.
              (tramp-send-command
               v (format (tramp-get-remote-mknod-or-mkfifo v) tmpstderr))
              ;; Create stderr process.
              (make-process
               :name (buffer-name stderr)
               :buffer stderr
               :command `("cat" ,tmpstderr)
               :coding coding
               :noquery t
               :filter nil
               :sentinel #'ignore
               :file-handler t))

            (while (get-process name1)
              ;; NAME must be unique as process name.
              (setq i (1+ i)
                    name1 (format "%s<%d>" name i)))
            (setq name name1)

            (with-tramp-saved-connection-properties
                v '("process-name"  "process-buffer")
              ;; Set the new process properties.
              (tramp-set-connection-property v "process-name" name)
              (tramp-set-connection-property v "process-buffer" buffer)
              (with-current-buffer (tramp-get-connection-buffer v)
                (unwind-protect
                    ;; We catch this event.  Otherwise, `make-process'
                    ;; could be called on the local host.
                    (save-excursion
                      (save-restriction
                        ;; Activate narrowing in order to save BUFFER
                        ;; contents.  Clear also the modification
                        ;; time; otherwise we might be interrupted by
                        ;; `verify-visited-file-modtime'.
                        (let ((buffer-undo-list t)
                              (inhibit-read-only t)
                              (mark (point-max))
                              (coding-system-for-write
                               (if (symbolp coding) coding (car coding)))
                              (coding-system-for-read
                               (if (symbolp coding) coding (cdr coding))))
                          (clear-visited-file-modtime)
                          (narrow-to-region (point-max) (point-max))
                          (catch 'suppress
                            ;; Set the pid of the remote shell.  This
                            ;; is needed when sending signals
                            ;; remotely.
                            (let ((pid
                                   (tramp-send-command-and-read v "echo $$")))
                              (setq p (tramp-get-connection-process v))
                              (process-put p 'remote-pid pid)
                              (tramp-set-connection-property
                               p "remote-pid" pid))
                            ;; disable carriage return to newline
                            ;; translation.  this does not work on
                            ;; macos, see bug#50748.
                            (when (and (memq connection-type '(nil pipe))
                                       (not
                                        (tramp-check-remote-uname v "darwin")))
                              (tramp-send-command v "stty -icrnl"))
                            ;; `tramp-maybe-open-connection' and
                            ;; `tramp-send-command-and-read' could
                            ;; have trashed the connection buffer.
                            ;; Remove this.
                            (widen)
                            (delete-region mark (point-max))
                            (narrow-to-region (point-max) (point-max))
                            ;; Now do it.
                            (if command
                                ;; Send the command.
                                (tramp-send-command v command nil t) ; nooutput
                              ;; Check, whether a pty is associated.
                              (unless (process-get p 'remote-tty)
                                (tramp-error
                                 v 'file-error
                                 "pty association is not supported for `%s'"
                                 name))))
                          ;; Set sentinel and filter.
                          (when sentinel
                            (set-process-sentinel p sentinel))
                          (when filter
                            (set-process-filter p filter))
                          (process-put p 'remote-command orig-command)
                          (tramp-set-connection-property
                           p "remote-command" orig-command)
                          ;; Set query flag and process marker for
                          ;; this process.  We ignore errors, because
                          ;; the process could have finished already.
                          (ignore-errors
                            (set-process-query-on-exit-flag p (null noquery))
                            (set-marker (process-mark p) (point)))
                          ;; We must flush them here already;
                          ;; otherwise `delete-file' will fail.
                          (tramp-flush-connection-property v "process-name")
                          (tramp-flush-connection-property v "process-buffer")
                          ;; Kill stderr process and delete named pipe.
                          (when (bufferp stderr)
                            (add-function
                             :after (process-sentinel p)
                             (lambda (_proc _msg)
                               (ignore-errors
                                 (while (accept-process-output
                                         (get-buffer-process stderr) 0 nil t))
                                 (delete-process (get-buffer-process stderr)))
                               (ignore-errors
                                 (delete-file remote-tmpstderr)))))
                          ;; Return process.
                          p)))

                  ;; Save exit.
                  (if (string-prefix-p tramp-temp-buffer-name (buffer-name))
                      (ignore-errors
                        (set-process-buffer p nil)
                        (kill-buffer (current-buffer)))
                    (set-buffer-modified-p bmp)))))))))))

(advice-add 'tramp-sh-handle-make-process :around #'tramp-sh-handle-make-process-override)

(defun font-size-hidpi ()
  (interactive)
  (doom-adjust-font-size 17 t '((doom-font . "Iosevka Custom Extended:regular")
                                (doom-variable-pitch-font . "Cantarell:medium")
                                (doom-serif-font . "Iosevka Custom:regular"))))
