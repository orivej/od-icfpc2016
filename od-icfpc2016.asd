(asdf:defsystem od-icfpc2016
  :depends-on (rutilsx let-plus vgplot)
  :serial t
  :components ((:file "package")
               (:file "point")
               (:file "problem")
               (:file "parse")
               (:file "plot")
               (:file "rotate")))
