(in-package :cl-user)
(defpackage dogechain-api-test-asd
  (:use :cl :asdf))
(in-package :dogechain-api-test-asd)

(defsystem dogechain-api-test
  :author "Phil Newton"
  :license "GPLv3"
  :depends-on (:dogechain-api
               :prove
               :cl-mock)
  :components ((:module "t"
                        :components
                        ((:test-file "dogechain-api"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
