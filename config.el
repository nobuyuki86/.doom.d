;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(setq user-full-name "nobuyuki"
      user-mail-address "nobuyuki86@outlook.jp")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'base16-sakura)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(cond (IS-LINUX
       (setq doom-font (font-spec :family "VLゴシック" :size 12)
             doom-variable-pitch-font (font-spec :family "VLPゴシック")
             doom-unicode-font (font-spec :family "VLゴシック")
             doom-big-font (font-spec :family "VLゴシック" :size 20)))
      (IS-WINDOWS
       (setq doom-font (font-spec :family "BIZ UDゴシック" :size 14)
             doom-variable-pitch-font (font-spec :family "BIZ UDPゴシック")
             doom-unicode-font (font-spec :family "BIZ UDゴシック")
             doom-big-font (font-spec :family "BIZ UDゴシック" :size 22))))

(setq completion-ignore-case t)


(setq scroll-conservatively 0)


(which-function-mode +1)


(toggle-frame-maximized)


;; character-code
(when IS-WINDOWS
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932))


(when IS-WINDOWS
  (global-set-key [remap doom/find-file-in-private-config] #'doom/open-private-config))


(when IS-WINDOWS
  (setq w32-rwindow-modifier nil
        w32-lwindow-modifier nil))


(use-package tr-ime
  :if IS-WINDOWS
  :init
  (setq default-input-method "W32-IME")
  :config
  (tr-ime-standard-install)
  (w32-ime-initialize))

(use-package mozc
  :if IS-LINUX
  :init
  (setq default-input-method "japanese-mozc"))


(use-package evil
  :init
  (setq evil-kill-on-visual-paste nil))


(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top))


(use-package prescient
  :config
  (setq vertico-sort-function #'prescient-sort)
  (advice-add #'vertico-insert :after
              (lambda ()
                (prescient-remember (vertico--candidate))))
  (prescient-persist-mode +1))


(use-package company
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0))

(use-package company-dwim
  :after company
  :bind (:map company-active-map
         ("TAB" . company-dwim-select-next)
         ("<tab>" . company-dwim-select-next)
         ("S-TAB" . company-dwim-select-previous)
         ("<backtab>" . company-dwim-select-previous)
         ("RET" . company-dwim-complete)
         ("<return>" . company-dwim-complete))
  :init
  (require 'company-dwim)

  (defun company-dwim-complete ()
    (interactive)
    (if company-dwim-half-committed
        (company-complete-selection)
      (newline-and-indent)))

  (setq company-selection-default -1)
  (add-to-list 'company-frontends 'company-dwim-frontend t)
  (delq 'company-preview-if-just-one-frontend company-frontends))

(use-package company-tabnine
  :after company
  :init
  (setq +company-backend-alist
        '((text-mode (:separate company-tabnine company-dabbrev company-yasnippet company-ispell))
          (prog-mode (:separate company-capf company-yasnippet company-tabnine))
          (conf-mode (:separate company-capf company-tabnine company-dabbrev-code company-yasnippet)))
        +lsp-company-backends
        '(:separate company-capf company-yasnippet company-tabnine)))

(use-package company-anywhere
  :after company)

(use-package company-prescient
  :after company
  :init
  (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode +1))


(use-package fussy
  :after company vertico
  :init
  (add-to-list 'completion-styles 'fussy t)
  (add-to-list '+vertico-company-completion-styles 'fussy t)

  (setq completion-category-defaults nil
        completion-category-overrides nil
        fussy-filter-fn #'fussy-filter-default)

  (use-package fuz-bin
    :init
    (setq fussy-score-fn #'fussy-fuz-bin-score)
    (fuz-bin-load-dyn))

  (with-eval-after-load 'company
    (defun j-company-capf (f &rest args)
      "Manage `completion-styles'."
      (let ((completion-styles '(fussy))
            (+vertico-company-completion-styles '(fussy))
            (fussy-max-candidate-limit 5000)
            (fussy-default-regex-fn #'fussy-pattern-first-letter)
            (fussy-prefer-prefix nil))
        (apply f args)))
    (advice-add 'company-capf :around 'j-company-capf)))


(use-package copilot
  :after company
  :bind (:map evil-insert-state-map
         ("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("C-TAB" . 'my-tab)
         ("C-<tab>" . 'my-tab)
         :map company-mode-map
         ("C-<tab>" . 'my-tab)
         ("C-TAB" . 'my-tab))
  :init
  (setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")

  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))

  (global-copilot-mode +1))


(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))

(use-package base16-theme
  :init
  (setq base16-sakura-theme-colors
        '(:base00 "#FEEEED" ;; 桜色
          :base01 "#E8D3D1" ;; 灰桜色
          :base02 "#D8C6BC" ;; 桜鼠
          :base03 "#9fa0a0" ;; 薄墨色
          :base04 "#006543" ;; 柚葉色
          :base05 "#2f2725" ;; 墨色
          :base06 "#405C36" ;; 老緑
          :base07 "#EEBBCB" ;; 撫子色
          :base08 "#E9546B" ;; 梅重
          :base09 "#C92E36" ;; 柘榴色
          :base0A "#0086AD" ;; 花色
          :base0B "#7BAA17" ;; 柳緑
          :base0C "#22825D" ;; 木賊色
          :base0D "#E2421F" ;; 紅葉色
          :base0E "#6967AB" ;; 竜胆色
          :base0F "#6A1435" ;; 紫檀色
          ))
  :config
  (load-theme 'base16-sakura t))


(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-bar-length 24)
  (nyan-mode +1))


(add-hook 'java-mode-hook (lambda ()
                            (setq-local tab-width 4
                                        c-basic-offset 4)))
