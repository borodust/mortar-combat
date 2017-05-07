(in-package :mortar-combat)


(defun fill-score-table (layout arena)
  (abandon-all layout)
  (loop for (name . score) in (score arena)
     do
       (adopt layout (make-text-label name :align :left))
       (adopt layout (make-text-label (format nil "~A" score) :align :right))))


(defmacro subscribe-to-click ((root name) &body body)
  (declare (ignore root))
  `(subscribe-body (button-click-event (poiu-button))
     (when (eq ,name (name-of poiu-button))
       (run (-> ((mortar-combat)) ()
              ,@body)))))


(defun make-ui (board)
  (enable-mouse-input board)
  (enable-character-input board)
  (enable-cursor-input board)
  (enable-keyboard-input board)

  (let ((main-menu (make-board-window board (vec2 300 150) 200 82 :hidden t))
        (arena-creation-dialog (make-board-window board (vec2 300 200) 200 145
                                                  :title "Enter arena name:"
                                                  :headerless nil
                                                  :hidden t))
        (game-menu (make-board-window board (vec2 20 236) 760 250 :hidden t))
        (combat-zone (make-board-window board (vec2 10 10) 780 580 :hidden t))
        (login-dialog (make-board-window board (vec2 300 200) 200 145
                                  :title "Enter your name:"
                                  :headerless nil)))
    (adopt-layout-by (main-menu)
      ((dynamic-row-layout 32 :columns 1)
       ((label-button "Combat zone" :name :combat-zone))
       ((label-button "Quit" :name :quit))))

    (adopt-layout-by (login-dialog)
      ((dynamic-row-layout 32)
       ((text-edit :name :nickname)))
      ((static-row-layout 16 1))
      ((dynamic-row-layout 32)
       ((label-button "Quit" :name :quit))
       ((spacing))
       ((label-button "Log in" :name :login))))

    (adopt-layout-by (arena-creation-dialog)
      ((dynamic-row-layout 32)
       ((text-edit :name :arena-name)))
      ((static-row-layout 16 1))
      ((dynamic-row-layout 32)
       ((label-button "Cancel" :name :arena-cancel))
       ((spacing))
       ((label-button "Go!" :name :arena-create))))

    (adopt-layout-by (game-menu)
      ((dynamic-row-layout 32)
       ((label-button "Leave arena" :name :leave))
       ((label-button "Quit" :name :quit)))
      ((dynamic-row-layout 26)
       ((text-label "Score:")))
      ((dynamic-row-layout 32 :columns 2 :name :score-table)))

    (adopt-layout-by (combat-zone)
      ((dynamic-row-layout 32)
       ((label-button "Create" :name :create))
       ((label-button "Join" :name :join))
       ((label-button "Refresh " :name :refresh))
       ((label-button "Main menu" :name :zone-to-main-menu)))
      ((dynamic-row-layout 32 :columns 1)
       ((text-label "Available arenas:"))
       ((list-select 32 :name :arena-list))))

    (let ((nickname-edit (find-element login-dialog :nickname))
          (arena-name-edit (find-element arena-creation-dialog :arena-name))
          (arena-list (find-element combat-zone :arena-list))
          (score-table (find-element game-menu :score-table))
          (selected-arena))
      (flet ((refresh-arena-list ()
               (run (>> (load-arena-list)
                        (-> ((mortar-combat)) (list)
                          (clear arena-list)
                          (dolist (name list)
                            (add-item arena-list name))))))
             (toggle-game-menu (ev)
               (when (and (eq (key-from ev) :escape)
                          (eq (state-from ev) :pressed))
                 (run (>> (-> ((mortar-combat)) ()
                            (let ((hidden-p (hiddenp game-menu)))
                              (if hidden-p
                                  (progn
                                    (fill-score-table score-table (arena-of *system*))
                                    (show-window game-menu))
                                  (hide-window game-menu))
                              hidden-p))
                          (-> ((host)) (was-hidden-p)
                            (if was-hidden-p
                                (unlock-cursor)
                                (lock-cursor))))))))

        (subscribe-to-click (login-dialog :login)
          (connect (text-of nickname-edit))
          (hide-window login-dialog)
          (show-window main-menu))

        (subscribe-to-click (main-menu :combat-zone)
          (refresh-arena-list)
          (hide-window main-menu)
          (show-window combat-zone))

        (subscribe-to-click (combat-zone :zone-to-main-menu)
          (hide-window combat-zone)
          (show-window main-menu))

        (subscribe-to-click (combat-zone :refresh)
          (refresh-arena-list))

        (subscribe-to-click (combat-zone :create)
          (hide-window combat-zone)
          (show-window arena-creation-dialog))

        (subscribe-to-click (combat-zone :join)
          (when selected-arena
            (post 'arena-join-requested :name selected-arena)
            (hide-window combat-zone)
            (subscribe 'keyboard-event #'toggle-game-menu)))

        (subscribe-to-click (arena-creation-dialog :arena-cancel)
          (hide-window arena-creation-dialog)
          (show-window combat-zone))

        (subscribe-to-click (arena-creation-dialog :arena-create)
          (post 'new-arena-requested :name (text-of arena-name-edit))
          (hide-window arena-creation-dialog)
          (subscribe 'keyboard-event #'toggle-game-menu))

        (subscribe-body (button-click-event (poiu-button))
          (when (eq :quit (name-of poiu-button))
            (unsubscribe 'keyboard-event #'toggle-game-menu)
            (hide-window game-menu) (show-window main-menu)
            (post 'arena-leave-requested))))

      (subscribe-body (item-selected (source item))
        (when (eq source arena-list)
          (setf selected-arena (item-name-of item)))))))
