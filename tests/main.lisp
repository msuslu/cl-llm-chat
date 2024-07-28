(defpackage llm-chat/tests/main
  (:use :cl
        :llm-chat
        :rove))
(in-package :llm-chat/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :llm-chat)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
