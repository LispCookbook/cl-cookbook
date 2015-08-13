//
// Example of creating a windows class in
// a DLL, then invoking it from Lisp.
//
// Convention for this example - routines
// that are written in C and are called by
// lisp are prefixed by "C_" and routines
// written in lisp called by C are prefixed
// by "Lisp_".  Note that, using LWW's FLI functions
// and declarations everything could be done in Lisp.
// This example shows how to bounce between Lisp and
// C in the event that one might wish to "reuse"
// existing C code.

//
// The routines C_CreateClass and C_CreateWindow are
// written in C and exported from the DLL to be called
// by Lisp.  Note that routines exported from a DLL
// are expected to use the :stdcall calling convention.
//
// The routine Lisp_WndProc is written in Lisp and
// exported (via FLI) so that it may be called
// by Windows.  Note that Windows expects Lisp_WndProc
// to use the :stdcall calling convention.

#include <windows.h>
#include <stdio.h>

BOOL APIENTRY DllMain( HANDLE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved )
{
  return TRUE;
}

// Windows calls a WndProc function to deliver a windows
// event to a widget.  In this example, this callback
// function is written in lisp.  If the WndProc function
// decides to handle the event, it should return 0, else
// it should call DefWindowProc.
//
// To get at the function, C uses a function pointer.
// Lisp must set this function pointer before calling
// C_CreateClass.  This variable is exported from
// the dll, allowing lisp to see it and to put a value
// into it.
//
// [Obviously, we could do this in other ways, for
// example, we could pass the pointer to C_CreateClass as
// a parameter.  Choosing to use a pointer variable shows
// off more of the FLI - the fact that lisp can manipulate
// C variables which reside in the DLL itself.]
//
__declspec(dllexport) LRESULT (CALLBACK *Lisp_WndProc)(HWND window,
UINT eventType, UINT wParam, LONG lParam);

//
// Example Window (ExWindow)
//

__declspec(dllexport) void C_CreateClass (void) {
WNDCLASS wClass;

/* setup the window class data and register the class */

  wClass.style = CS_HREDRAW | CS_VREDRAW;
  wClass.lpfnWndProc = Lisp_WndProc;
  wClass.cbClsExtra = wClass.cbWndExtra = 0;
  wClass.hInstance = GetModuleHandle (NULL);
  wClass.hIcon = LoadIcon(NULL,IDI_APPLICATION);
  wClass.hCursor = LoadCursor(NULL,IDC_ARROW);
  wClass.hbrBackground = GetStockObject(LTGRAY_BRUSH);
  wClass.lpszMenuName = NULL;
  wClass.lpszClassName = "ExWindow";
  RegisterClass(&wClass);
}

//
// create and display a window and returns its HWND
//
__declspec(dllexport) HWND C_CreateWindow (void) {
  HWND mainWindow;
  STARTUPINFO startup;
  mainWindow = CreateWindow("ExWindow",// window class name
                            "ExWindow", // Window caption
                            WS_OVERLAPPEDWINDOW, // style
                            CW_USEDEFAULT, // initial x position
                            CW_USEDEFAULT, // initial y position
                            CW_USEDEFAULT, // initial x extent
                            CW_USEDEFAULT, // initial y extent
                            NULL, //(HWND)parent, // parent window
                            NULL, // menu handle
                            GetModuleHandle (NULL), // program instance
handle
                            NULL); // creation parameters
  if (NULL == mainWindow) {
        long r = GetLastError();
  } else {
        GetStartupInfo (&startup);
        ShowWindow(mainWindow,startup.wShowWindow);
        UpdateWindow(mainWindow);
  }
  return mainWindow;
}
