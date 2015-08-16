(require "win-header")

(in-package :wh)
(export '(initialize-appendix-c))

; Following helps group the radio buttons
(defparameter *radio-list* nil)

; Define a message handler routine named testproc, inheriting
;  from the base MsgMap. Others could inherit from this one...
(DefMsgHandler appendix-c-proc MsgMap (:hbrbackground (1+ color_btnface))
               (WM_CREATE

                (DefGroupBox hwndGroupBox "Wish List" 25 30 300 70) ;; left top width height

                (DefPushButton hwndB1 "More Money" 30 48 90 40
                               ((mMessageBox "Quit buying Lisp books." "You requested more money.")))

                (DefPushButton hwndB2 "Youth" 121 48 90 40
                               ((mMessageBox "Sorry, not even Lisp can help you there." "You requested Youth.")))

                (DefPushButton hwndB3 "Better Lisp Skills" 212 48 90 40
                               ((mMessageBox "Keep practising." "You requested better Lisp skills.")))

                (DefRadioButton hwndRB1 "Lisp is cool" 40 105 100 20
                                :hwndlist *radio-list*)

                (DefRadioButton hwndRB2 "Lisp is boss" 200 105 100 20
                                :hwndlist *radio-list*)

                (SetCheck *hwndRB1*)

                (with-listview (DefListView hwndListView "" 25 150 300 200)
                  ((col "Lisp Book Title" 90)
                   (col "ISBN" 66)
                   (col "Author" 150)))

                (DefEdit hwndEdit "Learning Lisp is fun" 40 372 160 20 ())

                (DefAutoCheckBox hwndCheckBox "I like it" 210 372 70 20
                                 ((if (IsChecked *hwndCheckBox*)
                                      (mMessageBox "I'm glad you like it" "Atta boy!")
                                      (mMessageBox "Don't you like it?" "Whatsamatta U."))))
                0)

               (WM_PAINT
                (with-font "Arial" -20 font-normal 0 0 0 ;; points weight italic bold underline
                           (text-out "I Like Lisp" 70 5 :color (RGB 0 0 255)))
                (with-font "Arial" -11 font-bold 0 0 0
                           (text-out "Try it, you might like it, too!" 170 10
                                     :color (RGB 255 0 0)))
                0))

(defun initialize-appendix-c ()
  (DefActiveTopWindow "Test" 'appendix-c-proc
    :style ws_overlappedwindow :width 360 :height 450
    :title "First Lisp Screenshot"))
