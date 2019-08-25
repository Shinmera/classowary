(asdf:load-system :staple-markless)

(defpackage "class-docs"
  (:use #:cl)
  (:local-nicknames
   (#:cass #:org.shirakumo.classowary)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "class-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :classowary))))
  'page*)
