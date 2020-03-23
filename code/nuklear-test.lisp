(cl:defpackage :bodge-ui-window-test
  (:use :cl :bodge-ui :bodge-host))
(in-package :bodge-ui-window-test)

(defpanel (main-panel
            (:title "Hello Bodge UI")
            (:origin 200 50)
            (:width 400) (:height 400)
            (:options :movable :resizable
                      :minimizable :scrollable
                      :closable))
  (label :text "Nested widgets:")
  (horizontal-layout
   (radio-group
    (radio :label "Option 1")
    (radio :label "Option 2" :activated t))
   (vertical-layout
    (check-box :label "Check 1" :width 100)
    (check-box :label "Check 2"))
   (vertical-layout
    (label :text "Awesomely" :align :left)
    (label :text "Stacked" :align :centered)
    (label :text "Labels" :align :right)))
  (label :text "Expand by width:")
  (horizontal-layout
   (button :label "Dynamic")
   (button :label "Min-Width" :width 80)
   (button :label "Fixed-Width" :expandable nil :width 100))
  )

(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
