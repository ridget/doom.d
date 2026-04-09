;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; PERSONAL IDENTITY & VISUALS
;; ---------------------------------------------------------------------------
(setq user-full-name "Tom Ridge"
      user-mail-address "tomridge2@gmail.com")

(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")

;; Set Font size
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-big-font (font-spec :family "Fira Code" :size 24)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16))

;; Performance tweaks for bidirectional text
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;; ---------------------------------------------------------------------------
;; PROJECTILE
;; ---------------------------------------------------------------------------
(setq projectile-project-search-path '(("~/projects/" . 2))
      projectile-auto-update-cache t
      projectile-indexing-method 'hybrid)

;; ---------------------------------------------------------------------------
;; GLOBAL LSP CONFIGURATION (General UI & Performance)
;; ---------------------------------------------------------------------------
(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-height 70
        lsp-ui-doc-max-width 150
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-diagnostic-max-lines 20
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-enable t)

  (setq lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build" "_build" "deps" "postgres-data")))

;; ---------------------------------------------------------------------------
;; ELIXIR & HEEX CONFIGURATION
;; ---------------------------------------------------------------------------

;; Enable LSP for Elixir and HEEx Tree-sitter modes
(add-hook 'elixir-ts-mode-hook #'lsp-deferred)
(add-hook 'heex-ts-mode-hook #'lsp-deferred)

;; ElixirLS Specific Settings
(setq lsp-elixir-suggest-specs t
      lsp-elixir-dialyzer-enabled t
      lsp-elixir-signature-after-complete t
      lsp-elixir-enable-test-lenses t)

;; Workaround to enable running credo after lsp
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'elixir-mode 'elixir-ts-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (elixir-credo)))))))))

;; Fix duplicate "end" insertion in all Elixir modes
(after! smartparens
  (dolist (mode '(elixir-mode elixir-ts-mode))
    (sp-local-pair mode "for" "end" :actions nil)
    (sp-local-pair mode "if" "end" :actions nil)
    (sp-local-pair mode "case" "end" :actions nil)
    (sp-local-pair mode "cond" "end" :actions nil)
    (sp-local-pair mode "unless" "end" :actions nil)
    (sp-local-pair mode "with" "end" :actions nil)
    (sp-local-pair mode "try" "end" :actions nil)
    (sp-local-pair mode "fn" "end" :actions nil)
    (sp-local-pair mode "do" "end" :actions nil)
    (sp-local-pair mode "def" "end" :actions nil)
    (sp-local-pair mode "defp" "end" :actions nil)
    (sp-local-pair mode "defmodule" "end" :actions nil)
    (sp-local-pair mode "defimpl" "end" :actions nil)))

;; ---------------------------------------------------------------------------
;; WEB, TAILWIND & EMMET
;; ---------------------------------------------------------------------------
(use-package! lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t))

(after! web-mode
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-engines-alist nil))

(use-package! emmet-mode
  :hook (elixir-ts-mode . emmet-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'elixir-ts-mode)
  (setq emmet-expand-jsx-className? nil
        emmet-move-cursor-between-quotes t))

;; Emmet: Replace className with class in Elixir files
(defadvice! +emmet-elixir-classname-to-class-a (fn &rest args)
  :around #'emmet-make-html-tag
  (let ((result (apply fn args)))
    (if (derived-mode-p 'elixir-ts-mode)
        (replace-regexp-in-string " className=" " class=" result)
      result)))

(after! elixir-ts-mode
  (set-company-backend! 'elixir-ts-mode
    '(:separate company-emmet company-yasnippet company-capf))
  ;; Bind TAB to indent/expand
  (map! :map elixir-ts-mode-map
        :i [tab] #'+web/indent-or-yas-or-emmet-expand
        :i "TAB" #'+web/indent-or-yas-or-emmet-expand))

;; ---------------------------------------------------------------------------
;; TREE-SITTER TEXT OBJECTS & EVIL
;; ---------------------------------------------------------------------------
(require 'treesit)
(global-evil-matchit-mode 1)

;; Define Elixir "do...end" block selection
(defun +elixir/inner-do-block (count &optional beg end type)
  "Select the inner content of an Elixir do...end block."
  (interactive "p")
  (when-let* ((node (treesit-node-at (point)))
              (do-node (treesit-parent-until node (lambda (n) (equal (treesit-node-type n) "do_block")))))
    (let* ((children (treesit-node-children do-node))
           (start (treesit-node-end (car children)))
           (end (treesit-node-start (car (last children)))))
      (evil-range start end))))

(defun +elixir/outer-do-block (count &optional beg end type)
  "Select the entire Elixir do...end block."
  (interactive "p")
  (when-let* ((node (treesit-node-at (point)))
              (do-node (treesit-parent-until node (lambda (n) (equal (treesit-node-type n) "do_block")))))
    (evil-range (treesit-node-start do-node) (treesit-node-end do-node))))

;; Bindings for Elixir blocks and generic Tree-sitter calls
(after! elixir-ts-mode
  (map! :map elixir-ts-mode-map :textobj "b" #'+elixir/inner-do-block #'+elixir/outer-do-block)
  (define-key evil-inner-text-objects-map "b" #'+elixir/inner-do-block)
  (define-key evil-outer-text-objects-map "b" #'+elixir/outer-do-block))

(after! evil-textobj-tree-sitter
  (define-key evil-inner-text-objects-map "g" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-outer-text-objects-map "g" (evil-textobj-tree-sitter-get-textobj "call.outer")))

;; ---------------------------------------------------------------------------
;; OTHER LANGUAGES
;; ---------------------------------------------------------------------------
(setq org-babel-python-command "python3")


;; roblox luau
;; (after! projectile
;;   (projectile-register-project-type 'roblox-luau 
;;                                     '("default.project.json") 
;;                                     :project-file "default.project.json"
;;                                     :compile "lune run build"
;;                                     :test "lune run test"
;;                                     ;; Changed :run to :run-command for compatibility
;;                                     :run-command "rojo serve"
;;                                     :src-dir "src/"))

(use-package! eglot-luau
  :after eglot
  :init
  (setq eglot-luau-rojo-sourcemap-enabled t
        eglot-luau-rojo-sourcemap-includes-non-scripts t
        eglot-luau-auto-update-roblox-docs t
        eglot-luau-auto-update-roblox-types t
        eglot-luau-fflag-overrides '(("LuauSolverV2" "True")))
  :hook
  (lua-mode . eglot-luau-setup)
  (lua-mode . eglot-ensure))


(add-to-list 'auto-mode-alist '("\\.luau\\'" . lua-mode))

;; (use-package! mise
;;   :config
;;   (add-hook 'after-init-hook #'global-mise-mode))
