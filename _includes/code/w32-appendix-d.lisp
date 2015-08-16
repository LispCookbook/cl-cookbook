;
; Driver that initializes and calls a C dll
; to display a window.
;

; Define some Win32 types
(fli:define-c-typedef HWND (:unsigned :long))
(fli:define-c-typedef HANDLE (:pointer :void))
(fli:define-c-typedef HDC (:pointer :void))
(fli:define-c-typedef BOOL :int)
(fli:define-c-typedef wBYTE (:unsigned :char))
(fli:define-c-struct sRECT
  (left :LONG)
  (top  :LONG)
  (right :LONG)
  (bottom :LONG))
(fli:define-c-typedef RECT sRECT)
(fli:define-c-struct sPAINTSTRUCT ;; from winuser.h
  (hdc HDC)
  (fErase BOOL)
  (rcPaint RECT)
  (fRestore BOOL)
  (fIncUpdate BOOL)
  (rbgReserved (:c-array wBYTE 32)))
(fli:define-c-typedef PAINTSTRUCT sPAINTSTRUCT)

; some constants from WINUSER.H
(eval-when (compile load)
  (defconstant WM_COMMAND #x0111)
  (defconstant WM_PAINT #x000F)
  (defconstant WM_DESTROY #x0002))
(defconstant DT_TOP              #x00000000)
(defconstant DT_LEFT             #x00000000)
(defconstant DT_CENTER           #x00000001)
(defconstant DT_RIGHT            #x00000002)
(defconstant DT_VCENTER          #x00000004)
(defconstant DT_BOTTOM           #x00000008)
(defconstant DT_WORDBREAK        #x00000010)
(defconstant DT_SINGLELINE       #x00000020)
(defconstant DT_EXPANDTABS       #x00000040)
(defconstant DT_TABSTOP          #x00000080)
(defconstant DT_NOCLIP           #x00000100)
(defconstant DT_EXTERNALLEADING  #x00000200)
(defconstant DT_CALCRECT         #x00000400)
(defconstant DT_NOPREFIX         #x00000800)
(defconstant DT_INTERNAL         #x00001000)

; some Win32 calls
(fli:define-foreign-function (get-last-error "GetLastError")
    () :result-type :long :calling-convention :stdcall)

(fli:define-foreign-function (GetClientRect "GetClientRect")
    ((h HWND) (l (:pointer RECT)))
  :result-type :long :calling-convention :stdcall)

(fli:define-foreign-function (DrawText "DrawText" :dbcs)
    ((h HDC) (s :pointer) (nCount :int) (lprect :pointer)
     (uFormat (:unsigned :int)))
  :result-type :long :calling-convention :stdcall)

(fli:define-foreign-function (BeginPaint "BeginPaint")
    ((h HWND) (lpPaint :pointer))
  :result-type HDC :calling-convention :stdcall)

(fli:define-foreign-function (EndPaint "EndPaint")
    ((h HWND) (lpPaint :pointer))
  :result-type BOOL :calling-convention :stdcall)

(fli:define-foreign-function (DefWindowProc "DefWindowProc" :dbcs)
    ((h HWND) (msg (:unsigned :int))
     (wParam (:unsigned :int)) (lParam (:unsigned :int)))
  :result-type :long :calling-convention :stdcall)

;
; Exports from the DLL
;
; definition of pointer variable exported from the DLL
(fli:define-foreign-variable (Lisp_WndProc "Lisp_WndProc") :type
  :pointer)

; definition of routines exported from the DLL
(fli:define-foreign-function (C_CreateClass "C_CreateClass")
    () :result-type :void :calling-convention :stdcall)

(fli:define-foreign-function (C_CreateWindow "C_CreateWindow")
    () :result-type HWND :calling-convention :stdcall)

;
; Exports from Lisp
;

; The WndProc that Windows will call when an event is
; sent to the window.  Written in lisp, made callable
; from C/Windows (using :stdcall convention).
; The function closely resembles the hellowin.c code
; found in Petzhold (page 45, 5th ed), except that
; the "WM_CREATE" stuff is handled in C.
; (nb: we don't care what the name is - we use
; a pointer to this function)
(fli:define-foreign-callable
    ("func_Lisp_WndProc" :result-type :long
                         :calling-convention :stdcall)
    ((window HWND) (message (:unsigned :int))
     (wParam (:unsigned :int)) (lParam (:unsigned :int)))

  (case message

    (#.WM_PAINT
     (fli:with-dynamic-foreign-objects ((hdc HDC)
                                        (ps PAINTSTRUCT)
                                        (rect RECT))
       (fli:with-coerced-pointer (p-ps) ps
         (fli:with-coerced-pointer (p-rect) rect
                                   (setf hdc (BeginPaint window p-ps))
                                   (GetClientRect window p-rect)
                                   (fli:with-foreign-string
                                       (p-str ecount bcount :external-format (ext-format))
                                     "Hello" (declare (ignore ecount bcount))
                                     (DrawText hdc p-str -1 p-rect
                                               (logior DT_SINGLELINE DT_CENTER DT_VCENTER))
                                     (EndPaint window p-ps)
                                     0)))))

    (#.WM_DESTROY 0)  ;; if we use PostQuitMessage here, LWW will also
    exit

    (otherwise (DefWindowProc window message wParam lParam))))

(defun ext-format ()
  (if (string= (software-type) "Windows NT")
      :unicode
      :ascii))

;
; The test program
;

(defun run ()
  ; set up the DLL
  (fli:register-module "debug/wintest.dll")

  ; poke the function pointer for the lisp windows proc
  (setf (Lisp_WndProc)
        (fli:make-pointer :symbol-name "func_Lisp_WndProc"))

  ; create the Windows class for ExWindow
  (C_CreateClass)

  ; instantiate an ExWindow
  (C_CreateWindow)

  ; the windows loop takes care of the rest
  ; nb - we've hooked to the windows loop of LWW by
  ; using GetWindowHandle(0) in the C code.
  )
