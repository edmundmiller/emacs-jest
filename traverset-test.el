(require 'traverse.el)
(require 'ert)

(ert-deftest jest-test--check-bracket ()
  (should (equal nil (jest--check-open-bracket ?t)))
  (should (equal t (jest--check-open-bracket ?())))

(ert-deftest jest-test--check-quotes ()
  (should (equal nil (jest--check-char-quote ?t)))
  (should (equal t (jest--check-char-quote ?')))

(ert-deftest jest-test--remove-folded-range ()
  (should (equal "test text" (jest--remove-folded-range "test text"))))


(ert-deftest jest-test--remove-text-in-bracket ()
  (should (equal "test () text" (jest--remove-folded-range "test (be removed) text")))
  (should (equal "test {} text" (jest--remove-folded-range "test {be removed} text")))
  (should (equal "test [] text" (jest--remove-folded-range "test [be removed] text")))
  (should (equal "test <> text" (jest--remove-folded-range "test <be removed> text"))))

(ert-deftest jest-test--wont-remove-text-quotes ()
  (should (equal "test '(hi)' text" (jest--remove-folded-range "test '(hi)' text")))
  (should (equal "test \"<hi>\" text" (jest--remove-folded-range "test \"<hi>\" text")))
  (should (equal "test `<hi>` text" (jest--remove-folded-range "test `<hi>` text"))))
