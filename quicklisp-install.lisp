(load "http://beta.quicklisp.org/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload '(:quicklisp-slime-helper
		:cl-ppcre
		:arnesi
		:screamer
		:cxml
		:parenscript
		:kmrcl
		:cl-pdf
		:cl-mime
		:closer-mop
		:trivial-utf-8
		:unit-test
		:cl-gd
		:stem))
