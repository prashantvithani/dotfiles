;;; $DOOMDIR/custom-functions.el -*- lexical-binding: t; -*-

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
