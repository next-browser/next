# Keep in sync with https://github.com/flathub/engineer.atlas.Nyxt/.
app-id: engineer.atlas.Nyxt-WebKitGTK
runtime: org.gnome.Platform
runtime-version: '46'
sdk: org.gnome.Sdk
rename-icon: nyxt
rename-appdata-file: nyxt.metainfo.xml
rename-desktop-file: nyxt.desktop
command: nyxt

finish-args:
  - --device=dri
  - --share=network
  - --share=ipc
  - --socket=wayland
  - --socket=fallback-x11
  - --socket=pulseaudio
  - --socket=cups
  - --filesystem=host
  # Allow invoking host commands via flatpak-spawn --host <command> <command-args>
  - --talk-name=org.freedesktop.Flatpak
  # See http://sbcl.org/getting.html
  - --env=SBCL_HOME=/app/lib/sbcl

modules:
  - name: sbcl
    buildsystem: simple
    sources:
      - type: archive
        url: http://prdownloads.sourceforge.net/sbcl/sbcl-2.4.11-x86-64-linux-binary.tar.bz2
        sha256: e7c62c37f39a8b73663b575af0b2b733fd0a993b79a1a2262f81446e2a08b0f9
        only-arches: [x86_64]
    build-commands:
      - INSTALL_ROOT=${FLATPAK_DEST} sh install.sh

  - name: xsel
    buildsystem: autotools
    sources:
      - type: git
        url: https://github.com/kfish/xsel.git
        tag: 1.2.1

  - name: wl-clipboard
    buildsystem: meson
    config-opts:
      - -Dzshcompletiondir=no
      - -Dfishcompletiondir=no
    sources:
      - type: git
        url: https://github.com/bugaevc/wl-clipboard.git
        tag: v2.2.2

  - name: libfixposix
    buildsystem: autotools
    sources:
      - type: git
        url: https://github.com/sionescu/libfixposix.git
        tag: v0.5.2
      - type: script
        dest-filename: autogen.sh
        commands:
          - autoreconf -fi
    cleanup:
      - /lib/pkgconfig
      - /include
      - '*.h'
      - '*.pc'
      - '*.la'

  - name: nyxt
    buildsystem: simple
    sources:
      - type: dir
        path: .
    build-commands:
      - make install DESTDIR=${FLATPAK_DEST} PREFIX=

((nil . ((fill-column . 80)
         (project-vc-ignores . ("./_build"))))
 (org-mode . ((org-edit-src-content-indentation 0)))
 (lisp-mode
  . ((eval . (cl-flet ((enhance-imenu-lisp
                        (&rest keywords)
                        (dolist (keyword keywords)
                          (let ((prefix (when (listp keyword) (cl-second keyword)))
                                (keyword (if (listp keyword)
                                             (cl-first keyword)
                                           keyword)))
                            (add-to-list
                             'lisp-imenu-generic-expression
                             (list (purecopy (concat (capitalize keyword)
                                                     (if (string= (substring-no-properties keyword -1) "s")
                                                         "es"
                                                       "s")))
                                   (purecopy (concat "^\\s-*("
                                                     (regexp-opt
                                                      (list (if prefix
                                                                (concat prefix "-" keyword)
                                                              keyword)
                                                            (concat prefix "-" keyword))
                                                      t)
                                                     "\\s-+(\\(" lisp-mode-symbol-regexp "\\)"))
                                   2))))))
               ;; This adds the argument to the list of imenu known keywords.
               (enhance-imenu-lisp
                '("bookmarklet-command" "define")
                '("class" "define")
                '("command" "define")
                '("ffi-method" "define")
                '("ffi-generic" "define")
                '("function" "define")
                '("internal-page-command" "define")
                '("internal-page-command-global" "define")
                '("mode" "define")
                '("parenscript" "define")
                "defpsmacro"))))))
