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
(setq doom-theme 'modus-operandi)

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

(use-package emacs
  :init
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

  (setq completion-ignore-case t
        scroll-conservatively 0)

  (which-function-mode +1)

  (toggle-frame-maximized)

  ;; character-code
  (set-language-environment "Japanese")
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932)

  (when IS-WINDOWS
    (global-set-key [remap doom/find-file-in-private-config] #'doom/open-private-config))

  (when IS-WINDOWS
    (setq w32-rwindow-modifier nil
          w32-lwindow-modifier nil)))


(use-package tr-ime
  :if IS-WINDOWS
  :init
  (setq default-input-method "W32-IME")
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
  (define-key! 'global
    [remap indent-for-tab-command] 'company-indent-or-complete-common
    [remap c-indent-line-or-region] 'company-indent-or-complete-common)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0))

(use-package company-dwim
  :after company
  :config
  (define-key! company-active-map
    "TAB"       #'company-dwim-select-next
    "<tab>"     #'company-dwim-select-next
    "S-TAB"     #'company-dwim-select-previous
    "<backtab>" #'company-dwim-select-previous
    "RET"       #'company-dwim-complete-or-newline
    "<return>"  #'company-dwim-complete-or-newline)

  (setq company-selection-default nil)
  (add-to-list 'company-frontends 'company-dwim-frontend t)
  (delq 'company-preview-if-just-one-frontend company-frontends))

(use-package company-tabnine
  :after company
  :init
  (setq +company-backend-alist
        '((text-mode (:separate company-tabnine company-dabbrev company-yasnippet company-ispell))
          (prog-mode (:separate company-capf company-yasnippet company-tabnine company-dabbrev))
          (conf-mode (:separate company-capf company-tabnine company-dabbrev-code company-yasnippet)))
        +lsp-company-backends
        '(:separate company-capf company-yasnippet company-tabnine company-dabbrev)))

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
  (setq completion-styles '(fussy)
        +vertico-company-completion-styles '(fussy)
        completion-category-defaults nil
        completion-category-overrides nil
        fussy-filter-fn #'fussy-filter-default)

  (with-eval-after-load 'company
    (defun j-company-capf (f &rest args)
      "Manage `completion-styles'."
      (let ((fussy-max-candidate-limit 5000)
            (fussy-default-regex-fn #'fussy-pattern-first-letter)
            (fussy-prefer-prefix nil))
        (apply f args)))
    (advice-add 'company-capf :around 'j-company-capf)))

(use-package fzf-native
  :after fussy
  :init
  (setq fussy-score-fn 'fussy-fzf-native-score)
  (fzf-native-load-dyn))


(use-package copilot
  :after company
  :init
  (setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")

  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))

  (map! :i
        "C-RET"      #'copilot-accept-completion-by-word
        "C-<return>" #'copilot-accept-completion-by-word
        :map company-active-map
        "C-RET"      #'my-tab
        "C-<return>" #'my-tab
        :map company-mode-map
        "C-<return>" #'my-tab
        "C-RET"      #'my-tab)

  (global-copilot-mode +1))


(use-package lsp-pyright
  :init
  (add-hook! 'python-mode-hook (lambda ()
                                 (require 'lsp-pyright))))

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-bar-length 24)
  (nyan-mode +1))


(use-package! expand-region
  :init
  (map! :nv "SPC v" #'er/expand-region))


(use-package modus-themes
  :init
  ;; Override the given associations
  ;;; The Modus themes

  ;; These are built into Emacs 28 or higher, though I use the package for
  ;; my development purposes (I need to make sure it always builds cleanly
  ;; and works properly).

  ;; Read their manual with Emacs' Info reader, or visit:
  ;; <https://protesilaos.com/emacs/modus-themes>.
  (require 'modus-themes)

  ;; Add all your customizations prior to loading the themes
  ;;
  ;; NOTE: these are not my preferences!  I am always testing various
  ;; configurations.  Though I still like what I have here.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented nil
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers nil

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line '(accented)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup nil

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax nil

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line nil

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat',
        ;; `accented', `faint', `variable-pitch', `underline',
        ;; `all-buttons', the symbol of any font weight as listed in
        ;; `modus-themes-weights', and a floating point number
        ;; (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons nil

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts nil

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also', WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions
        '((matches . (semibold))
          (selection . (extrabold accented))
          (popup . (extrabold accented)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(accented no-extend)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs nil

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch light 1.6))
          (header-date . (underline-today grayscale workaholic 1.2))
          (event . (accented italic varied))
          (scheduled . rainbow)
          (habit . simplified))

        ;; The `modus-themes-headings' is an alist with lots of possible
        ;; combinations, include per-heading-level tweaks: read the
        ;; manual or its doc string
        modus-themes-headings
        '((0 . (variable-pitch light (height 2.2)))
          (1 . (rainbow variable-pitch light (height 1.6)))
          (2 . (rainbow variable-pitch light (height 1.4)))
          (3 . (rainbow variable-pitch regular (height 1.3)))
          (4 . (rainbow regular (height 1.2)))
          (5 . (rainbow (height 1.1)))
          (t . (variable-pitch extrabold))))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)

  ;; Load the theme of your preference:
  (modus-themes-load-operandi) ; OR (modus-themes-load-vivendi)

  ;; Optionally set the `modus-themes-toggle' to a key binding:
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;;; Modus themes "Summertime"

  ;; Read the relevant blog post:
  ;; <https://protesilaos.com/codelog/2022-07-26-modus-themes-color-override-demo/>

  ;; Thanks to user "Summer Emacs" for (i) suggesting the name
  ;; "summertime", (ii) testing variants of this in her setup, and (iii)
  ;; sending me feedback on possible tweaks and refinements.  All errors
  ;; are my own.  (This information is shared with permission.)
  (define-minor-mode modus-themes-summertime
    "Refashion the Modus themes by overriding their colors.

This is a complete technology demonstration to show how to
manually override the colors of the Modus themes.  I have taken
good care of those overrides to make them work as a fully fledged
color scheme that is compatible with all user options of the
Modus themes.

These overrides are usable by those who (i) like something more
fancy than the comparatively austere looks of the Modus themes,
and (ii) can cope with a lower contrast ratio.

The overrides are set up as a minor mode, so that the user can
activate the effect on demand.  Those who want to load the
overrides at all times can either add them directly to their
configuration or enable `modus-themes-summertime' BEFORE loading
either of the Modus themes (if the overrides are evaluated after
the theme, the theme must be reloaded).

Remember that all changes to theme-related variables require a
reload of the theme to take effect (the Modus themes have lots of
user options, apart from those overrides).

The `modus-themes-summertime' IS NOT an official extension to the
Modus themes and DOES NOT comply with its lofty accessibility
standards.  It is included in the official manual as guidance for
those who want to make use of the color overriding facility we
provide."
    :init-value nil
    :global t
    (if modus-themes-summertime
        (setq modus-themes-operandi-color-overrides
              '((bg-main . "#fff0f2")
                (bg-dim . "#fbe6ef")
                (bg-alt . "#f5dae6")
                (bg-hl-line . "#fad8e3")
                (bg-active . "#efcadf")
                (bg-inactive . "#f3ddef")
                (bg-active-accent . "#ffbbef")
                (bg-region . "#dfc5d1")
                (bg-region-accent . "#efbfef")
                (bg-region-accent-subtle . "#ffd6ef")
                (bg-header . "#edd3e0")
                (bg-tab-active . "#ffeff2")
                (bg-tab-inactive . "#f8d3ef")
                (bg-tab-inactive-accent . "#ffd9f5")
                (bg-tab-inactive-alt . "#e5c0d5")
                (bg-tab-inactive-alt-accent . "#f3cce0")
                (fg-main . "#543f78")
                (fg-dim . "#5f476f")
                (fg-alt . "#7f6f99")
                (fg-unfocused . "#8f6f9f")
                (fg-active . "#563068")
                (fg-inactive . "#8a5698")
                (fg-docstring . "#5f5fa7")
                (fg-comment-yellow . "#a9534f")
                (fg-escape-char-construct . "#8b207f")
                (fg-escape-char-backslash . "#a06d00")
                (bg-special-cold . "#d3e0f4")
                (bg-special-faint-cold . "#e0efff")
                (bg-special-mild . "#c4ede0")
                (bg-special-faint-mild . "#e0f0ea")
                (bg-special-warm . "#efd0c4")
                (bg-special-faint-warm . "#ffe4da")
                (bg-special-calm . "#f0d3ea")
                (bg-special-faint-calm . "#fadff9")
                (fg-special-cold . "#405fb8")
                (fg-special-mild . "#407f74")
                (fg-special-warm . "#9d6f4f")
                (fg-special-calm . "#af509f")
                (bg-completion . "#ffc5e5")
                (bg-completion-subtle . "#f7cfef")
                (red . "#ed2f44")
                (red-alt . "#e0403d")
                (red-alt-other . "#e04059")
                (red-faint . "#ed4f44")
                (red-alt-faint . "#e0603d")
                (red-alt-other-faint . "#e06059")
                (green . "#217a3c")
                (green-alt . "#417a1c")
                (green-alt-other . "#006f3c")
                (green-faint . "#318a4c")
                (green-alt-faint . "#518a2c")
                (green-alt-other-faint . "#20885c")
                (yellow . "#b06202")
                (yellow-alt . "#a95642")
                (yellow-alt-other . "#a06f42")
                (yellow-faint . "#b07232")
                (yellow-alt-faint . "#a96642")
                (yellow-alt-other-faint . "#a08042")
                (blue . "#275ccf")
                (blue-alt . "#475cc0")
                (blue-alt-other . "#3340ef")
                (blue-faint . "#476ce0")
                (blue-alt-faint . "#575ccf")
                (blue-alt-other-faint . "#3f60d7")
                (magenta . "#bf317f")
                (magenta-alt . "#d033c0")
                (magenta-alt-other . "#844fe4")
                (magenta-faint . "#bf517f")
                (magenta-alt-faint . "#d053c0")
                (magenta-alt-other-faint . "#846fe4")
                (cyan . "#007a9f")
                (cyan-alt . "#3f709f")
                (cyan-alt-other . "#107f7f")
                (cyan-faint . "#108aaf")
                (cyan-alt-faint . "#3f80af")
                (cyan-alt-other-faint . "#3088af")
                (red-active . "#cd2f44")
                (green-active . "#116a6c")
                (yellow-active . "#993602")
                (blue-active . "#475ccf")
                (magenta-active . "#7f2ccf")
                (cyan-active . "#007a8f")
                (red-nuanced-bg . "#ffdbd0")
                (red-nuanced-fg . "#ed6f74")
                (green-nuanced-bg . "#dcf0dd")
                (green-nuanced-fg . "#3f9a4c")
                (yellow-nuanced-bg . "#fff3aa")
                (yellow-nuanced-fg . "#b47232")
                (blue-nuanced-bg . "#e3e3ff")
                (blue-nuanced-fg . "#201f6f")
                (magenta-nuanced-bg . "#fdd0ff")
                (magenta-nuanced-fg . "#c0527f")
                (cyan-nuanced-bg . "#dbefff")
                (cyan-nuanced-fg . "#0f3f60")
                (bg-diff-heading . "#b7cfe0")
                (fg-diff-heading . "#041645")
                (bg-diff-added . "#d6f0d6")
                (fg-diff-added . "#004520")
                (bg-diff-changed . "#fcefcf")
                (fg-diff-changed . "#524200")
                (bg-diff-removed . "#ffe0ef")
                (fg-diff-removed . "#891626")
                (bg-diff-refine-added . "#84cfa4")
                (fg-diff-refine-added . "#002a00")
                (bg-diff-refine-changed . "#cccf8f")
                (fg-diff-refine-changed . "#302010")
                (bg-diff-refine-removed . "#da92b0")
                (fg-diff-refine-removed . "#500010")
                (bg-diff-focus-added . "#a6e5c6")
                (fg-diff-focus-added . "#002c00")
                (bg-diff-focus-changed . "#ecdfbf")
                (fg-diff-focus-changed . "#392900")
                (bg-diff-focus-removed . "#efbbcf")
                (fg-diff-focus-removed . "#5a0010"))
              modus-themes-vivendi-color-overrides
              '((bg-main . "#25152a")
                (bg-dim . "#2a1930")
                (bg-alt . "#382443")
                (bg-hl-line . "#332650")
                (bg-active . "#463358")
                (bg-inactive . "#2d1f3a")
                (bg-active-accent . "#50308f")
                (bg-region . "#5d4a67")
                (bg-region-accent . "#60509f")
                (bg-region-accent-subtle . "#3f285f")
                (bg-header . "#3a2543")
                (bg-tab-active . "#26162f")
                (bg-tab-inactive . "#362647")
                (bg-tab-inactive-accent . "#36265a")
                (bg-tab-inactive-alt . "#3e2f5a")
                (bg-tab-inactive-alt-accent . "#3e2f6f")
                (fg-main . "#debfe0")
                (fg-dim . "#d0b0da")
                (fg-alt . "#ae85af")
                (fg-unfocused . "#8e7f9f")
                (fg-active . "#cfbfef")
                (fg-inactive . "#b0a0c0")
                (fg-docstring . "#c8d9f7")
                (fg-comment-yellow . "#cf9a70")
                (fg-escape-char-construct . "#ff75aa")
                (fg-escape-char-backslash . "#dbab40")
                (bg-special-cold . "#2a3f58")
                (bg-special-faint-cold . "#1e283f")
                (bg-special-mild . "#0f3f31")
                (bg-special-faint-mild . "#0f281f")
                (bg-special-warm . "#44331f")
                (bg-special-faint-warm . "#372213")
                (bg-special-calm . "#4a314f")
                (bg-special-faint-calm . "#3a223f")
                (fg-special-cold . "#c0b0ff")
                (fg-special-mild . "#bfe0cf")
                (fg-special-warm . "#edc0a6")
                (fg-special-calm . "#ff9fdf")
                (bg-completion . "#502d70")
                (bg-completion-subtle . "#451d65")
                (red . "#ff5f6f")
                (red-alt . "#ff8f6d")
                (red-alt-other . "#ff6f9d")
                (red-faint . "#ffa0a0")
                (red-alt-faint . "#f5aa80")
                (red-alt-other-faint . "#ff9fbf")
                (green . "#51ca5c")
                (green-alt . "#71ca3c")
                (green-alt-other . "#51ca9c")
                (green-faint . "#78bf78")
                (green-alt-faint . "#99b56f")
                (green-alt-other-faint . "#88bf99")
                (yellow . "#f0b262")
                (yellow-alt . "#f0e242")
                (yellow-alt-other . "#d0a272")
                (yellow-faint . "#d2b580")
                (yellow-alt-faint . "#cabf77")
                (yellow-alt-other-faint . "#d0ba95")
                (blue . "#778cff")
                (blue-alt . "#8f90ff")
                (blue-alt-other . "#8380ff")
                (blue-faint . "#82b0ec")
                (blue-alt-faint . "#a0acef")
                (blue-alt-other-faint . "#80b2f0")
                (magenta . "#ff70cf")
                (magenta-alt . "#ff77f0")
                (magenta-alt-other . "#ca7fff")
                (magenta-faint . "#e0b2d6")
                (magenta-alt-faint . "#ef9fe4")
                (magenta-alt-other-faint . "#cfa6ff")
                (cyan . "#30cacf")
                (cyan-alt . "#60caff")
                (cyan-alt-other . "#40b79f")
                (cyan-faint . "#90c4ed")
                (cyan-alt-faint . "#a0bfdf")
                (cyan-alt-other-faint . "#a4d0bb")
                (red-active . "#ff6059")
                (green-active . "#64dc64")
                (yellow-active . "#ffac80")
                (blue-active . "#4fafff")
                (magenta-active . "#cf88ff")
                (cyan-active . "#50d3d0")
                (red-nuanced-bg . "#440a1f")
                (red-nuanced-fg . "#ffcccc")
                (green-nuanced-bg . "#002904")
                (green-nuanced-fg . "#b8e2b8")
                (yellow-nuanced-bg . "#422000")
                (yellow-nuanced-fg . "#dfdfb0")
                (blue-nuanced-bg . "#1f1f5f")
                (blue-nuanced-fg . "#bfd9ff")
                (magenta-nuanced-bg . "#431641")
                (magenta-nuanced-fg . "#e5cfef")
                (cyan-nuanced-bg . "#042f49")
                (cyan-nuanced-fg . "#a8e5e5")
                (bg-diff-heading . "#304466")
                (fg-diff-heading . "#dae7ff")
                (bg-diff-added . "#0a383a")
                (fg-diff-added . "#94ba94")
                (bg-diff-changed . "#2a2000")
                (fg-diff-changed . "#b0ba9f")
                (bg-diff-removed . "#50163f")
                (fg-diff-removed . "#c6adaa")
                (bg-diff-refine-added . "#006a46")
                (fg-diff-refine-added . "#e0f6e0")
                (bg-diff-refine-changed . "#585800")
                (fg-diff-refine-changed . "#ffffcc")
                (bg-diff-refine-removed . "#952838")
                (fg-diff-refine-removed . "#ffd9eb")
                (bg-diff-focus-added . "#1d4c3f")
                (fg-diff-focus-added . "#b4dfb4")
                (bg-diff-focus-changed . "#424200")
                (fg-diff-focus-changed . "#d0daaf")
                (bg-diff-focus-removed . "#6f0f39")
                (fg-diff-focus-removed . "#eebdba")))
      (setq modus-themes-operandi-color-overrides nil
            modus-themes-vivendi-color-overrides nil)))

  ;; Load `modus-operandi' while disabling all others
  (modus-themes-summertime)
  (modus-themes-load-operandi))

