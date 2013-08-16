(asdf:defsystem :mailbox
  :author "Lucien Pullen <drurowin@gmail.com>"
  :description "Simple multithreading mailboxes."
  :license "MIT"
  :depends-on (:bordeaux-threads)
  :components ((:file "mailbox")))
