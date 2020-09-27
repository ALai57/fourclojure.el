;;; fourclojure-test.el --- Tests for fourclojure

(ert-deftest flatten-test/basic ()
  (with-sandbox
   (should (equal (flatten '("a" "b" ("c"))) "abc"))))

(ert-deftest flatten-test/nested-maps ()
  (with-sandbox
   (should (equal (flatten '(("a" "b" ("c")))) "abc"))))

(ert-deftest flatten-test/ignored-fields ()
  (with-sandbox
   (should (equal (flatten '((table "b" ("c")))) ""))
   (should (equal (flatten '((div . "b"))) ""))
   (should (equal (flatten '((div "b"))) "b"))))

(ert-deftest extract-4clojure-test-cases/happy-path ()
  (should
   (equal
    '("(= 3 ((__ nth) 2 [1 2 3 4 5]))"
      "(= true ((__ >) 7 8))"
      "(= 4 ((__ quot) 2 8))"
      "(= [1 2 3] ((__ take) [1 2 3 4 5] 3))")
    (extract-4clojure-test-cases
     (with-temp-buffer
       (insert-file-contents "./resources/example_4c_problem.html")
       (libxml-parse-html-region (point-min) (point-max)))))))

(ert-deftest dom->4clojure-problems/happy-path ()
  (let ((all-problems
         (dom->4clojure-problems
          (with-temp-buffer
            (insert-file-contents "./resources/4c_problem_list.html")
            (libxml-parse-html-region (point-min)
                                      (point-max))))))
    (should (eq 156 (length all-problems)))
    (should (equal '("Nothing but the Truth" "\\\"/problem/1\\\"" "Elementary")
                   (car all-problems)))))

;;; fourclojure-test.el ends here
