pragma Ada_2022;

with Ada.Strings.Text_Buffers;

with Interfaces.C; use Interfaces.C;
with System;

package AWTK.Windows is

   type Window_Class_Styles is
     (REDRAW_ON_CLIENT_AREA_CHANGE_VERTICALLY,
      REDRAW_ON_CLIENT_AREA_CHANGE_HORIZONTALLY,
      UNDEFINED_1,
      RECEIVE_DOUBLE_CLICK_MESSAGES,
      UNDEFINED_2,
      ALLOCATE_DEVICE_CONTEXT_FOR_EACH_WINDOW,
      ALLOCATE_SHARED_DEVICE_CONTEXT,
      ALLOCATE_PARENT_CLIPPING_DEVICE_CONTEXT,
      UNDEFINED_3,
      DISABLE_CLOSE_BUTTON,
      UNDEFINED_4,
      SAVE_OCCLUDED_SCREEN_AS_BITMAP,
      ALIGN_CLIENT_AREA_ON_BOUNDARY_HORIZONTALLY,
      ALIGN_WINDOW_ON_BOUNDARY_HORIZONTALLY,
      APPLICATION_GLOBAL_CLASS,
      UNDEFINED_5);

   for Window_Class_Styles use
     (REDRAW_ON_CLIENT_AREA_CHANGE_VERTICALLY    => 2 ** 0,
      REDRAW_ON_CLIENT_AREA_CHANGE_HORIZONTALLY  => 2 ** 1,
      UNDEFINED_1                                => 2 ** 2,
      RECEIVE_DOUBLE_CLICK_MESSAGES              => 2 ** 3,
      UNDEFINED_2                                => 2 ** 4,
      ALLOCATE_DEVICE_CONTEXT_FOR_EACH_WINDOW    => 2 ** 5,
      ALLOCATE_SHARED_DEVICE_CONTEXT             => 2 ** 6,
      ALLOCATE_PARENT_CLIPPING_DEVICE_CONTEXT    => 2 ** 7,
      UNDEFINED_3                                => 2 ** 8,
      DISABLE_CLOSE_BUTTON                       => 2 ** 9,
      UNDEFINED_4                                => 2 ** 10,
      SAVE_OCCLUDED_SCREEN_AS_BITMAP             => 2 ** 11,
      ALIGN_CLIENT_AREA_ON_BOUNDARY_HORIZONTALLY => 2 ** 12,
      ALIGN_WINDOW_ON_BOUNDARY_HORIZONTALLY      => 2 ** 13,
      APPLICATION_GLOBAL_CLASS                   => 2 ** 14,
      UNDEFINED_5                                => 2 ** 15);

   type Window_Class_Styles_Flags is
     array (Window_Class_Styles'First .. Window_Class_Styles'Last) of Boolean
   with
     Put_Image => Window_Class_Styles_Flags_Put_Image,
     Component_Size => 1,
     Size => unsigned'Size;
   procedure Window_Class_Styles_Flags_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Window_Class_Styles_Flags);

   type WORD is new Interfaces.Unsigned_16;
   type DWORD is new Interfaces.Unsigned_32;
   type ATOM is new WORD;
   type BOOL is new int;
   type UINT is new unsigned;
   subtype HANDLE is System.Address;
   type HWND is new HANDLE;
   type HMENU is new HANDLE;
   type HINSTANCE is new HANDLE;
   subtype HMODULE is HINSTANCE;
   type LPCSTR is new HANDLE;
   type LPWSTR is new HANDLE;
   type LPTSTR is new LPWSTR;
   type LONG_PTR is mod System.Memory_Size;
   type LRESULT is new LONG_PTR;
   type WPARAM is new LONG_PTR;
   type LPARAM is new LONG_PTR;

   type Window_Callback_Message is
     (UNKNOWN,
      CREATED,
      DESTROYING,
      MOVED,
      SIZE_CHANGED,
      ACTIVATION_CHANGED,
      KEYBOARD_FOCUS_CHANGED,
      ERASE_BACKGROUND_REQUEST,
      VISIBILITY_CHANGED,
      ACTIVATION_CHANGED_EXTL,
      SIZE_OVERRIDE_REQUEST,
      OBJECT_REQUEST,
      POSITION_CHANGING,
      POSITION_CHANGED,
      ICON_REQUEST,
      CONFIRM_CREATION,
      AFTER_DESTROYING,
      CALCULATE_CLIENT_AREA_SIZE,
      FRAME_PAINT_REQUEST,
      ACTIVATION_CHANGED_INTL,
      IME_SET_CONTEXT)
   with Size => UINT'Size;

   for Window_Callback_Message use
     (UNKNOWN                    => 16#00_00#,
      CREATED                    => 16#00_01#,
      DESTROYING                 => 16#00_02#,
      MOVED                      => 16#00_03#,
      SIZE_CHANGED               => 16#00_05#,
      ACTIVATION_CHANGED         => 16#00_06#,
      KEYBOARD_FOCUS_CHANGED     => 16#00_07#,
      ERASE_BACKGROUND_REQUEST   => 16#00_14#,
      VISIBILITY_CHANGED         => 16#00_18#,
      ACTIVATION_CHANGED_EXTL    => 16#00_1C#,
      SIZE_OVERRIDE_REQUEST      => 16#00_24#,
      OBJECT_REQUEST             => 16#00_3D#,
      POSITION_CHANGING          => 16#00_46#,
      POSITION_CHANGED           => 16#00_47#,
      ICON_REQUEST               => 16#00_7F#,
      CONFIRM_CREATION           => 16#00_81#,
      AFTER_DESTROYING           => 16#00_82#,
      CALCULATE_CLIENT_AREA_SIZE => 16#00_83#,
      FRAME_PAINT_REQUEST        => 16#00_85#,
      ACTIVATION_CHANGED_INTL    => 16#00_86#,
      IME_SET_CONTEXT            => 16#02_81#);

   type Window_Class is record
      Structure_Size     : unsigned := Window_Class'Size / char'Size;
      Class_Styles       : Window_Class_Styles_Flags := [];
      Window_Procedure   : HANDLE;
      Class_Size_Extra   : int := 0;
      Window_Size_Extra  : int := 0;
      Window_Handle      : HINSTANCE;
      Icon_Handle        : HANDLE := System.Null_Address;
      Cursor_Handle      : HANDLE := System.Null_Address;
      Brush_Handle       : HANDLE := System.Null_Address;
      Menu_Resource_Name : LPCSTR := LPCSTR (System.Null_Address);
      Window_Class_Name  : LPCSTR;
      Small_Icon_Handle  : HANDLE := System.Null_Address;
   end record
   with Convention => C;

   function Get_Module_Handle_A
     (Module_Name : LPCSTR := LPCSTR (System.Null_Address)) return HMODULE
   with Import => True, External_Name => "GetModuleHandleA", Convention => C;

   function Register_Class_ExA (Class_Pointer : HANDLE) return ATOM
   with Import => True, External_Name => "RegisterClassExA", Convention => C;

   function Create_Window_ExA
     (Extended_Style      : DWORD;
      Class_Name          : LPCSTR;
      Window_Name         : LPCSTR;
      Style               : DWORD;
      X, Y, Width, Height : int;
      Parent_Window       : HWND := HWND (System.Null_Address);
      Menu_Handle         : HMENU := HMENU (System.Null_Address);
      Module_Handle       : HINSTANCE := HINSTANCE (System.Null_Address);
      Message_LPParam     : HANDLE := System.Null_Address) return HWND
   with Import => True, External_Name => "CreateWindowExA", Convention => C;

   function Format_Message_W
     (Parameters              : DWORD;
      Message_Source          : HANDLE := System.Null_Address;
      Mesasge_ID, Language_ID : DWORD;
      Buffer                  : LPTSTR;
      Buffer_Size             : DWORD;
      Arguments               : HANDLE := System.Null_Address) return DWORD
   with Import => True, External_Name => "FormatMessageW", Convention => C;

   function Get_Last_Error return DWORD
   with Import => True, External_Name => "GetLastError", Convention => C;

   function Get_Last_Error_Formatted return Wide_String;

   function Windows_Process_Callback
     (Window_Handle : HWND;
      Raw_Message   : UINT;
      AdditionalW   : WPARAM;
      AdditionalL   : LPARAM) return LRESULT
   with Export => True, Convention => C;

   function Default_Process_Callback_A
     (Window_Handle : HWND;
      Raw_Message   : UINT;
      AdditionalW   : WPARAM;
      AdditionalL   : LPARAM) return LRESULT
   with Import => True, External_Name => "DefWindowProcA", Convention => C;

   function Get_Message_W
     (Message        : HANDLE;
      Window_Filter  : HANDLE := System.Null_Address;
      Filter_ID_Low  : UINT := 0;
      Filter_ID_High : UINT := 0) return BOOL
   with Import => True, External_Name => "GetMessageW", Convention => C;

   function Translate_Message (Message : HANDLE) return BOOL
   with Import => True, External_Name => "TranslateMessage", Convention => C;

   function Dispatch_Message_W (Message : HANDLE) return LRESULT
   with Import => True, External_Name => "DispatchMessageW", Convention => C;

   type Windows_Window is new Window with null record;
   function Create_Windows_Window return Windows_Window;
end AWTK.Windows;
