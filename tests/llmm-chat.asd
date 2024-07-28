(in-package #:asdf-user)

(defsystem "llm-chat"
  :version "0.0.1"
  :author "Mehmet Suslu"
  :license "No licence"
  :depends-on ("dexador" "cl-json")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "llm-chat/tests"))))

(defsystem "llm-chat/tests"
  :author "Mehmet Suslu"
  :license ""
  :depends-on ("llm-chat"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for llm-chat"
  :perform (test-op (op c) (symbol-call :rove :run c)))
