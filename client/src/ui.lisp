(in-package :mortar-combat)


(defun make-ui (context board)
  (enable-mouse-input board)
  (enable-character-input board)
  (enable-cursor-input board)
  (enable-keyboard-input board)
  (let ((login (make-board-window board 300 200 200 145
                                  :title "Enter your name:"
                                  :headerless nil)))
    (adopt-layout-by (login)
      ((dynamic-row-layout 32)
       ((text-edit :name :nickname)))
      ((static-row-layout 16 1))
      ((dynamic-row-layout 32)
       ((label-button "Quit" :name :quit))
       ((spacing))
       ((label-button "Log in" :name :login))))

    (let ((nickname-edit (find-element login :nickname)))
      (subscribe-body-to (button-click-event (poiu-button)) (events)
        (case (name-of poiu-button)
          (:login (connect (text-of nickname-edit)))
          (:quit (post (make-exit-requested) (events))))))))
