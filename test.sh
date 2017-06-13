cd "$(basename $0)"

sbcl --no-userinit \
  --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --load /home/edwlan/quicklisp/setup.lisp \
  --eval '(ql:quickload :'$(basename $PWD)')' \
  --eval '(vector-update-stream::test)' \
  --quit
