#!/usr/bin/env bb

(require '[clojure.java.shell :refer [sh with-sh-dir]])

;; Get sha256 base64 hash for source code on github
(let [[owner name version] *command-line-args*
      ext ".tar.gz"
      url (format "https://github.com/%s/%s/archive/%s%s" owner name version ext)
      extraced (str name "-" version)]
  (println "Downloading from:" url)
  (with-sh-dir "/tmp"
    ;; Get tarball
    (sh "wget" "-q" url)
    ;; Extract tarball
    (sh "tar" "-xf" (str version ext))
    ;; Delete tarball
    (sh "rm" (str version ext))
    ;; Print hash
    (print (:out (sh "nix" "hash" "path" "--type" "sha256" "--sri" extraced)))
    ;; Remove extraced directory
    (sh "rm" "-rf" extraced)))
