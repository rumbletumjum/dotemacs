(use-package rust-mode
  :ensure t
  :commands rust-mode)

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode)
         (racer-mode. eldoc-mode))
  :config
  (setq racer-rust-src-path
        "/Users/ron/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

(provide 'rtj-rust)
