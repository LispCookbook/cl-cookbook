;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Header-file kinds of code
;

; Determine character type
(defun external-format ()
  (if (string= (software-type) "Windows NT")
      :unicode
      :ascii))

(defparameter *class-name* "LWTestClass")

;--- Win32 SDK Constants

; Not all of these are verified by testing in code
; More are included than are used to allow play
(defconstant COLOR_WINDOW                 5)
(defconstant CS_HREDRAW                   1)
(defconstant CS_VREDRAW                   2)
(defconstant CW_USEDEFAULT       #x80000000)
(defconstant IDC_ARROW                32512)
(defconstant SW_SHOW                      5)
(defconstant WM_CLOSE            #x00000010)
(defconstant WM_DESTROY          #x00000002)
(defconstant WM_PAINT            #x0000000f)
(defconstant WS_BORDER           #x00080000)
(defconstant WS_CAPTION          #x00C00000)
(defconstant WS_CHILD            #x40000000)
(defconstant WS_DISABLED         #x08000000)
(defconstant WS_EX_CONTROLPARENT #x00010000)
(defconstant WS_EX_APPWINDOW     #x00040000)
(defconstant WS_MAXIMIZEBOX      #x00010000)
(defconstant WS_MINIMIZEBOX      #x00020000)
(defconstant WS_OVERLAPPED       #x00000000)
(defconstant WS_POPUP            #x80000000)
(defconstant WS_SYSMENU          #x00080000)
(defconstant WS_THICKFRAME       #x00040000)
(defconstant WS_VISIBLE          #x10000000)

; Aggregates
(defconstant WS_POPUPWINDOW (logior ws_popup ws_border ws_sysmenu))
(defconstant WS_OVERLAPPEDWINDOW (logior ws_overlapped ws_caption
                                         ws_sysmenu ws_thickframe
                                         ws_minimizebox ws_maximizebox))

;--- Win32 SDK data types

(fli:define-c-typedef BOOL (:unsigned :long))
(fli:define-c-typedef DWORD (:unsigned :long))
(fli:define-c-typedef HANDLE (:unsigned :long))
(fli:define-c-typedef HDC (:unsigned :long))
(fli:define-c-typedef HINSTANCE (:unsigned :long))
(fli:define-c-typedef HMENU (:unsigned :long))
(fli:define-c-typedef HWND (:unsigned :long))
(fli:define-c-typedef INT :int)
(fli:define-c-typedef LONG :long)
(fli:define-c-typedef LPCTSTR :pointer)
(fli:define-c-typedef LPSTR :pointer)
(fli:define-c-typedef LPVOID :long)
(fli:define-c-typedef LPFN :long) ;; Doesn't work as :pointer
(fli:define-c-typedef UINT (:unsigned :int))
(fli:define-c-typedef ULONG (:unsigned :long))
(fli:define-c-typedef wBYTE (:unsigned :char))
(fli:define-c-typedef PTR (:unsigned :long))

;--- Win32 SDK structures

; POINT
(fli:define-c-struct sPOINT
    (x int)
  (y int))
(fli:define-c-typedef POINT sPOINT)

; MSG
(fli:define-c-struct sMSG
    (hwnd hwnd)
  (wParam ulong)
  (lParam ulong)
  (time dword)
  (point point))
(fli:define-c-typedef MSG sMSG)

; PAINTSTRUCT
(fli:define-c-struct sPAINTSTRUCT
    (HDC hdc)
  (fErase bool)
  (rcPaint-x uint)
  (rcPaint-y uint)
  (rcPaint-width uint)
  (rcPaint-height uint)
  (fRestore bool)
  (fIncUpdate bool)
  (rgbReserved (:c-array wBYTE 32)))
(fli:define-c-typedef PAINTSTRUCT sPAINTSTRUCT)

; WndClass
(fli:define-c-struct sWNDCLASS
    (style uint)
  (lpfnWndProc lpfn)
  (cbClsExtra int)
  (cbWndExtra int)
  (hInstance handle)
  (hIcon handle)
  (hCursor handle)
  (hBrBackground handle)
  (lpszMenuName ulong) ;; ulong so it can be set to null without error
  (lpszClassName lpctstr))
(fli:define-c-typedef WNDCLASS sWNDCLASS)

;--- Win32 SDK functions

; BeginPaint
(fli:define-foreign-function
    (BeginPaint "BeginPaint" :calling-convention :stdcall)
    ((hwnd hwnd) (lpPaintStruct ptr))
  :result-type hdc)

; CreateWindowEx
(fli:define-foreign-function
    (CreateWindowEx "CreateWindowEx" :dbcs :calling-convention :stdcall)
    ((dwExStyle dword) (lpClassName lpctstr) (lpWindowName lpctstr)
     (dwStyle dword) (x uint) (y uint) (nWidth uint) (nHeight uint)
     (hwndParent hwnd) (hMenu hmenu) (hInstance hinstance) (lpParam lpvoid))
  :result-type hwnd)

; DefWindowProc
(fli:define-foreign-function
    (DefWindowProc "DefWindowProc" :dbcs :calling-convenction :stdcall)
    ((hwnd ulong) (msg ulong) (wparam ulong) (lparam ulong))
  :result-type ulong)

; DispatchMessage
(fli:define-foreign-function
    (DispatchMessage "DispatchMessage" :dbcs :calling-convention :stdcall)
    ((MsgPtr ptr)) ;; We're passing a pointer but Lisp doesn't know
  :result-type ulong)

; EndPaint
(fli:define-foreign-function
    (EndPaint "EndPaint" :calling-convention :stdcall)
    ((hwnd hwnd) (lpPaintStruct ptr))
  :result-type bool)

; GetLastError
(fli:define-foreign-function
    (GetLastError "GetLastError" :calling-convention :stdcall)
    ()
  :result-type dword)

; GetMessage
(fli:define-foreign-function
    (GetMessage "GetMessage" :dbcs :calling-convention :stdcall)
    ((lpMsg ptr) (hwnd ulong) (MsgFiltMin ulong) (MsgFiltMax ulong))
  :result-type bool)

; GetModuleHandle - with no module name specified; pass this a zero i.e. NULL
(fli:define-foreign-function
    (GetModuleHandle-current "GetModuleHandle"
                             :dbcs :calling-convenction :stdcall)
    ((lpModuleName :long))
  :result-type handle)

; Lisp helper
(defmacro current-module-handle ()
  `(GetModuleHandle-current 0))

; GetModuleHandle - pass this a foreign string naming the desired module
(fli:define-foreign-function
    (GetModuleHandle "GetModuleHandle" :dbcs :calling-convenction :stdcall)
    ((lpModuleName lpctstr))
  :result-type handle)

; LoadCursor
(fli:define-foreign-function
    (LoadCursor "LoadCursor" :dbcs)
    ((hInstance handle) (param (:unsigned :long)))
  :result-type handle)

; PostQuitMessage
(fli:define-foreign-function
    (PostQuitMessage "PostQuitMessage" :calling-convention :stdcall)
    ((nExitCode :int))
  :result-type :void)

; RegisterClass
(fli:define-foreign-function
    (RegisterClass "RegisterClass" :dbcs :calling-convention :stdcall)
    ((WndClass ptr))
  :result-type bool)

; TextOut
(fli:define-foreign-function
    (TextOut "TextOut" :dbcs :calling-convention :stdcall)
    ((HDC hdc) (nXStart uint) (nYStart uint)
     (lpString lpctstr) (cbString uint))
  :result-type bool)

; TranslateMessage
(fli:define-foreign-function
    (TranslateMessage "TranslateMessage" :calling-convention :stdcall)
    ((msg-ptr ulong))
  :result-type bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Code specific to one program
;

;--- Win32 SDK Callbacks

; WndProc -- Window procedure for the window we will create
(fli:define-foreign-callable
    (wndproc :result-type :long :calling-convention :stdcall)
    ((hwnd hwnd) (msg (:unsigned :long))
     (wparam (:unsigned :long)) (lparam (:unsigned :long)))
  (case msg
    (#.WM_PAINT (wndproc-paint hwnd msg wparam lparam))
    #+console (#.WM_DESTROY (PostQuitMessage 0) 0)
    (t (DefWindowProc hwnd msg wparam lparam))))

;--- Functions called from WndProc

(defun wndproc-paint (hwnd msg wparam lparam)
  (fli:with-dynamic-foreign-objects ()
    (let* ((ps (fli:allocate-dynamic-foreign-object :type 'paintstruct))
           (hdc (beginpaint hwnd (fli:pointer-address ps))))
                                        ; Paint here
      (fli:with-foreign-string (text-ptr ec bc :external-format (external-format))
        "Hello, Lisp!"
        (textout hdc 0 0 text-ptr ec))
      (endpaint hwnd (fli:pointer-address ps))
      0)))

;--- Main Functions

(defun register-class ()
  (fli:with-foreign-string (cn-p ec bc :external-format (external-format))
    *class-name*
    ;; Below - use with-dynamic... for automatic freeing. Make some pointers.
    (let (rslt
          (wc-p (fli:allocate-foreign-object :type 'wndclass))
          (wp-p (fli:make-pointer :symbol-name "wndproc")))
      (unwind-protect
           (progn
             (setf (fli:foreign-slot-value wc-p 'style) (logior cs_hredraw cs_vredraw))
             (setf (fli:foreign-slot-value wc-p 'lpfnWndProc)
                   (fli:pointer-address wp-p))
             (setf (fli:foreign-slot-value wc-p 'cbClsExtra) 0)
             (setf (fli:foreign-slot-value wc-p 'cbWndExtra) 0)
             (setf (fli:foreign-slot-value wc-p 'hInstance) (current-module-handle))
             (setf (fli:foreign-slot-value wc-p 'hIcon) 0)
             (setf (fli:foreign-slot-value wc-p 'hCursor) (LoadCursor 0 IDC_ARROW))
             (setf (fli:foreign-slot-value wc-p 'hbrBackground) (1+ color_window))
             (setf (fli:foreign-slot-value wc-p 'lpszMenuName) 0)
             (setf (fli:foreign-slot-value wc-p 'lpszClassName) cn-p)
             (setq rslt (RegisterClass (fli:pointer-address wc-p))))
        (fli:free-foreign-object wp-p)
        (fli:free-foreign-object wc-p)
        (if (/= rslt 0)
            rslt
            (error (format nil "~&RegisterClass failed in reg-class. Error: ~a"
                           (GetLastError))))))))

(defvar *reg-class-atom* (register-class))

; CreateWindow
(defun create-toplevel-window-run (window-name)
  "See create-window."
  (fli:with-foreign-string ;; class name pointer
      (cn-p ec bc :external-format (external-format)) *class-name*
      (fli:with-foreign-string ;; window name pointer
          (wn-p ec bc :external-format (external-format)) window-name
          (let ((hwnd (createwindowex 0 cn-p wn-p
                                      (logior ws_visible ws_overlappedwindow)
                                      cw_usedefault cw_usedefault cw_usedefault cw_usedefault
                                        ;0 0 640 480
                                      0 0 (GetModuleHandle-current 0) 0)))
            (when (zerop hwnd)
              (error (format nil "CreateWindow failed. GetLastError is ~a"
                             (GetLastError))))
            #-console hwnd ;; Should return wParam in thread, what about here?
            #+console ;; Message pump the thread
            (fli:with-dynamic-foreign-objects ()
	      (let ((ms (fli:allocate-dynamic-foreign-object :type 'msg)))
                (do ((gm (GetMessage (fli:pointer-address ms) 0 0 0)
                         (GetMessage (fli:pointer-address ms) 0 0 0)))
                    ((zerop gm) (fli:foreign-slot-value ms 'wParam))
                  (TranslateMessage (fli:pointer-address ms))
                  (DispatchMessage (fli:pointer-address ms)))))))))

; CloseWindow
(defun close-window (hwnd)
  (sendmessage hwnd WM_CLOSE 0 0))

(defmacro create-toplevel-window (window-name)
  "Creates an overlapped window with title WINDOW-NAME.
	      Returns the window handle."
  #-console `(create-toplevel-window-run ,window-name)
  #+console  `(progn
                (when (null (mp:list-all-processes))
                  (mp:initialize-multiprocessing))
                (mp:process-run-function ,window-name nil
                                         'create-toplevel-window-run ,window-name)))
