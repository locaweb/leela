;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil (lice:default-license . "mit"))
 (python-mode . ((py-indent-offset . 4)))
 ("src/warpdrive" . ((nil . ((dgvncsz0f-compile-root . "src/warpdrive")
                             (dgvncsz0f-compile-command . "cabal compile")
                             (dgvncsz0f-compile-test-command . "cabal test")
                             (dgvncsz0f-compile-clean-command . "cabal clean")))))
 ("src/storaged" . ((nil . ((dgvncsz0f-compile-root . "src/storaged")
                            (dgvncsz0f-compile-command . "lein with-profile storaged,triggers compile")
                            (dgvncsz0f-compile-test-command . "lein with-profile storaged,triggers test")
                            (dgvncsz0f-compile-clean-command . "lein with-profile storaged,triggers clean"))))))
