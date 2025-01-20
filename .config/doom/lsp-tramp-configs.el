;;; lsp-tramp-configs.el --- Configs for LSP over tramp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Prashant Vithani

;; Author: Prashant Vithani <prashantvithani@gmail.com>

(after! lsp-solargraph
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     #'lsp-solargraph--build-command)
    :major-modes '(ruby-mode enh-ruby-mode)
    :priority -1
    :remote? t
    :multi-root lsp-solargraph-multi-root
    :library-folders-fn (lambda (_workspace) lsp-solargraph-library-directories)
    :server-id 'ruby-ls-remote
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "solargraph")))))))

(after! lsp-pylsp
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda () lsp-pylsp-server-command))
    :major-modes '(python-mode cython-mode)
    :priority -1
    :remote? t
    :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
    :server-id 'pylsp-remote
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "pylsp")))))))
(after! lsp-go
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
    :remote? t
    :activation-fn (lsp-activate-on "go" "go.mod")
    :language-id "go"
    :priority 0
    :server-id 'gopls-remote
    :completion-in-comments? t
    :library-folders-fn #'lsp-go--library-default-directories
    :after-open-fn (lambda ()
                     ;; https://github.com/golang/tools/commit/b2d8b0336
                     (setq-local lsp-completion-filter-on-incomplete nil)))))


;; (after! rustic-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection
;;                      (lambda () rustic-lsp-server))
;;     :major-modes '(rustic-mode)
;;     :priority -1
;;     :remote? t
;;     :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
;;     :server-id 'rustic-lsp-remote
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                         (lsp--set-configuration
;;                          (lsp-configuration-section "rust")))))))

(after! lsp-metals
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "metals-emacs")
    :major-modes '(scala-mode)
    :priority -1
    :initialization-options '((decorationProvider . t)
                              (inlineDecorationProvider . t)
                              (didFocusProvider . t)
                              (executeClientCommandProvider . t)
                              (doctorProvider . "html")
                              (statusBarProvider . "on")
                              (debuggingProvider . t)
                              (treeViewProvider . t)
                              (quickPickProvider . t)
                              (inputBoxProvider . t))
    :notification-handlers (ht ("metals/executeClientCommand" #'lsp-metals--execute-client-command)
                               ("metals/publishDecorations" #'lsp-metals--publish-decorations)
                               ("metals/treeViewDidChange" #'lsp-metals-treeview--did-change)
                               ("metals-model-refresh" #'lsp-metals--model-refresh)
                               ("metals/status" #'lsp-metals--status-string))
    :request-handlers (ht ("metals/quickPick" #'lsp-metals--quick-pick)
                          ("metals/inputBox" #'lsp-metals--input-box))
    :action-handlers (ht ("metals-debug-session-start" (-partial #'lsp-metals--debug-start :json-false))
                         ("metals-run-session-start" (-partial #'lsp-metals--debug-start t)))
    :server-id 'metals-remote
    :remote? t
    :initialized-fn (lambda (workspace)
                      (lsp-metals--add-focus-hooks)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "metals"))))
    :after-open-fn (lambda ()
                     (add-hook 'lsp-on-idle-hook #'lsp-metals--did-focus nil t))
    :completion-in-comments? t
    ;; :download-server-fn #'lsp-metals--download-server
    )))

(after! lsp-java
  (when (file-remote-p default-directory)
    (setq lsp-java-workspace-dir (concat (file-remote-p default-directory) "~/.jdtls/workspace")
          dap-java-test-runner (concat (file-remote-p default-directory) "~/.jdtls/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar")
          lsp-java-server-install-dir (concat (file-remote-p default-directory) "~/.jdtls/eclipse.jdt.ls/")))
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-tramp-connection #'lsp-java--ls-command)
    :major-modes '(java-mode jdee-mode)
    :server-id 'jdtls-remote
    :multi-root t
    :remote? t
    :notification-handlers (ht ("language/status" #'lsp-java--language-status-callback)
                               ("language/actionableNotification" #'lsp-java--actionable-notification-callback)
                               ("language/progressReport" #'lsp-java--progress-report)
                               ("workspace/notify" #'lsp-java--workspace-notify)
                               ("language/eventNotification" #'ignore))
    :request-handlers (ht ("workspace/executeClientCommand" 'lsp-java-boot--workspace-execute-client-command))
    :action-handlers (ht ("java.apply.workspaceEdit" #'lsp-java--apply-workspace-edit)
                         ("java.action.generateToStringPrompt" #'lsp-java--action-generate-to-string)
                         ("java.action.hashCodeEqualsPrompt" #'lsp-java--action-generate-equals-and-hash-code)
                         ("java.action.organizeImports" #'lsp-java--action-organize-imports)
                         ("java.action.overrideMethodsPrompt" #'lsp-java--override-methods-prompt)
                         ("java.action.generateAccessorsPrompt" #'lsp-java--generate-accessors-prompt)
                         ("java.action.generateConstructorsPrompt" #'lsp-java--generate-constructors-prompt)
                         ("java.action.applyRefactoringCommand" #'lsp-java--apply-refactoring-command)
                         ("java.action.rename" #'lsp-java--action-rename)
                         ("java.show.references" #'lsp-java--show-references)
                         ("java.show.implementations" #'lsp-java--show-implementations))
    :uri-handlers (ht ("jdt" #'lsp-java--resolve-uri))
    :initialization-options (lambda ()
                              (list :settings (lsp-configuration-section "java")
                                    :extendedClientCapabilities
                                    (list :progressReportProvider (lsp-json-bool lsp-java-progress-reports-enabled)
                                          :classFileContentsSupport t
                                          :classFileContentsSupport t
                                          :overrideMethodsPromptSupport t
                                          :hashCodeEqualsPromptSupport t
                                          :advancedOrganizeImportsSupport t
                                          :generateConstructorsPromptSupport t
                                          :generateToStringPromptSupport t
                                          :advancedGenerateAccessorsSupport t
                                          :advancedExtractRefactoringSupport t
                                          :moveRefactoringSupport t
                                          :resolveAdditionalTextEditsSupport t)
                                    :bundles (lsp-java--bundles)
                                    :workspaceFolders (->> (lsp-session)
                                                           lsp-session-server-id->folders
                                                           (gethash 'jdtls)
                                                           (-uniq)
                                                           (-map #'lsp--path-to-uri)
                                                           (apply #'vector))))
    :library-folders-fn (lambda (_workspace) (list lsp-java-workspace-cache-dir))
    :before-file-open-fn (lambda (_workspace)
                           (let ((metadata-file-name (lsp-java--get-metadata-location buffer-file-name)))
                             (setq-local lsp-buffer-uri
                                         (when (file-exists-p metadata-file-name)
                                           (with-temp-buffer (insert-file-contents metadata-file-name)
                                                             (buffer-string))))))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "java"))
                        (lsp--server-register-capability
                         ;; (lsp-make-registration
                         ;;          :id "test-id"
                         ;;  :method "workspace/didChangeWatchedFiles"
                         ;;  :register-options? (lsp-make-did-change-watched-files-registration-options
                         ;;                      :watchers
                         ;;                      (vector (lsp-make-file-system-watcher :glob-pattern "**/*.java")
                         ;;                              (lsp-make-file-system-watcher :glob-pattern "**/pom.xml")
                         ;;                              (lsp-make-file-system-watcher :glob-pattern "**/*.gradle")
                         ;;                              (lsp-make-file-system-watcher :glob-pattern "**/.project")
                         ;;                              (lsp-make-file-system-watcher :glob-pattern "**/.classpath")
                         ;;                              (lsp-make-file-system-watcher :glob-pattern "**/settings/*.prefs"))))
                         )))
    :completion-in-comments? t

    :download-server-fn #'lsp-java--ensure-server)))
