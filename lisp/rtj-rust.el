(use-package rust-mode
  :ensure t
  :commands rust-mode)

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode))
  :config
  (setq racer-rust-src-path
        "/Users/ron/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  (add-hook 'racer-mode-hook #'eldoc-mode))

(provide 'rtj-rust)
