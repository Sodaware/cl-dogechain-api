(load "~/.sbclrc")
(load "dogechain-api.asd")
(load "dogechain-api-test.asd")
(asdf:test-system :dogechain-api)
