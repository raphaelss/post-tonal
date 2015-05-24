(in-package :asdf-user)

(defsystem "post-tonal"
  :description "Library implementing concepts of post-tonal theory"
  :version "0.0.1"
  :author "Raphael Santos <contact@raphaelss.com>"
  :license "AGPLv3 License"
  :components ((:file "post-tonal")
               (:file "pitch" :depends-on ("post-tonal"))))
