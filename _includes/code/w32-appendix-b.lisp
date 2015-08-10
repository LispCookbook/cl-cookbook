(require "win-header")

(in-package :wh)
(export '(initialize-appendix-b))

(DefMsgHandler appendix-b-proc MsgMap ()
               (WM_PAINT
                (text-out "Hello, Lisp!" 0 0 :color (RGB 0 0 255))
                0))

(defun initialize-appendix-b ()
  (DefActiveTopWindow "Hello" 'appendix-b-proc
    :style ws_overlappedwindow :width 200 :height 100 :title "Hello Program"))
