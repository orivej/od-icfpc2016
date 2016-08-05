(asdf:defsystem od-icfpc2016
  :depends-on (rutilsx let-plus vgplot)
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "parse")
               (:file "plot")
               (:file "main")))
