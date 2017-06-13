;;;; vector-update-stream.asd

(asdf:defsystem #:vector-update-stream
  :description "A stream that updates a user-provided backing vector, based on flexi-streams"
  :author "Ed Langley <fiddlerwoaroof@gmail.com"
  :license "BSD-2-CLAUSE"
  :depends-on (#:trivial-gray-streams
	       #:flexi-streams
               #:alexandria
               #:serapeum)
  :serial t
  :components ((:file "package")
               (:file "stream-to-vector")))

