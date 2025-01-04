pragma Ada_2022;

with Interfaces.C; use Interfaces.C;
with System;

package AWTK.Windows is

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
   type HRESULT is new unsigned_long;
   type WPARAM is new LONG_PTR;
   type LPARAM is new LONG_PTR;
   type LPMSG is new HANDLE;
   type POINT is record
      X, Y : long;
   end record
   with Convention => C;
   subtype POINTL is POINT;

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
   with Component_Size => 1, Size => UINT'Size, Convention => C;

   type Extended_Window_Class_Styles is
     (DOUBLE_BORDER_MODAL_FRAME,
      UNDEFINED_1,
      DO_NOT_NOTIFY_PARENT_OF_STATE,
      ALWAYS_ON_TOP,
      ACCEPT_DRAG_DROPPED_FILES,
      TRANSPARENT_BACKGROUND,
      MDI_CHILD_WINDOW,
      FLOATING_TOOLBAR,
      BORDER_WITH_RAISED_EDGE,
      BORDER_WITH_SUNKEN_EDGE,
      HAS_HELP_BUTTON,
      UNDEFINED_2,
      HA_RIGHT_ALIGNED_PROPERTIES,
      HA_RIGHT_TO_LEFT_READING,
      HA_LEFT_SCROLLBAR,
      UNDEFINED_3,
      CHILD_CONTROLLED_NAVIGATION,
      BORDER_3D_NO_INPUT,
      TASKBAR_WINDOW,
      LAYERED_WINDOW,
      LAYOUT_IS_NOT_INHERITED,
      NO_SCREEN_REDIRECTION_BITMAP,
      HA_LAYOUT_RIGHT_TO_LEFT,
      UNDEFINED_4,
      UNDEFINED_5,
      DOUBLE_BUFFERED_BOTTOM_TO_TOP_RENDERING,
      UNDEFINED_6,
      NOT_ACTIVATABLE_BY_INPUT,
      UNDEFINED_7,
      UNDEFINED_8,
      UNDEFINED_9,
      UNDEFINED_10);

   for Extended_Window_Class_Styles use
     (DOUBLE_BORDER_MODAL_FRAME               => 2 ** 0,
      UNDEFINED_1                             => 2 ** 1,
      DO_NOT_NOTIFY_PARENT_OF_STATE           => 2 ** 2,
      ALWAYS_ON_TOP                           => 2 ** 3,
      ACCEPT_DRAG_DROPPED_FILES               => 2 ** 4,
      TRANSPARENT_BACKGROUND                  => 2 ** 5,
      MDI_CHILD_WINDOW                        => 2 ** 6,
      FLOATING_TOOLBAR                        => 2 ** 7,
      BORDER_WITH_RAISED_EDGE                 => 2 ** 8,
      BORDER_WITH_SUNKEN_EDGE                 => 2 ** 9,
      HAS_HELP_BUTTON                         => 2 ** 10,
      UNDEFINED_2                             => 2 ** 11,
      HA_RIGHT_ALIGNED_PROPERTIES             => 2 ** 12,
      HA_RIGHT_TO_LEFT_READING                => 2 ** 13,
      HA_LEFT_SCROLLBAR                       => 2 ** 14,
      UNDEFINED_3                             => 2 ** 15,
      CHILD_CONTROLLED_NAVIGATION             => 2 ** 16,
      BORDER_3D_NO_INPUT                      => 2 ** 17,
      TASKBAR_WINDOW                          => 2 ** 18,
      LAYERED_WINDOW                          => 2 ** 19,
      LAYOUT_IS_NOT_INHERITED                 => 2 ** 20,
      NO_SCREEN_REDIRECTION_BITMAP            => 2 ** 21,
      HA_LAYOUT_RIGHT_TO_LEFT                 => 2 ** 22,
      UNDEFINED_4                             => 2 ** 23,
      UNDEFINED_5                             => 2 ** 24,
      DOUBLE_BUFFERED_BOTTOM_TO_TOP_RENDERING => 2 ** 25,
      UNDEFINED_6                             => 2 ** 26,
      NOT_ACTIVATABLE_BY_INPUT                => 2 ** 27,
      UNDEFINED_7                             => 2 ** 28,
      UNDEFINED_8                             => 2 ** 29,
      UNDEFINED_9                             => 2 ** 30,
      UNDEFINED_10                            => 2 ** 31);

   type Extended_Window_Class_Styles_Flags is
     array (Extended_Window_Class_Styles'First
            .. Extended_Window_Class_Styles'Last)
     of Boolean
   with Component_Size => 1, Size => DWORD'Size, Convention => C;

   OVERLAPPED_WINDOW : constant Extended_Window_Class_Styles_Flags :=
     [BORDER_WITH_RAISED_EDGE => True,
      BORDER_WITH_SUNKEN_EDGE => True,
      others                  => False];
   PALETTE_WINDOW    : constant Extended_Window_Class_Styles_Flags :=
     [BORDER_WITH_RAISED_EDGE => True,
      FLOATING_TOOLBAR        => True,
      ALWAYS_ON_TOP           => True,
      others                  => False];

   type COM_Concurrency_Models is
     (MULTITHREADED,
      APARTMENT_THREADED,
      DISABLE_OLE1DDE,
      SPEED_OVER_MEMORY);

   for COM_Concurrency_Models use
     (MULTITHREADED               => 0,
      APARTMENT_THREADED                             => 2 ** 1,
      DISABLE_OLE1DDE           => 2 ** 2,
      SPEED_OVER_MEMORY                           => 2 ** 3);

   type COM_Concurrency_Models_Flags is
     array (COM_Concurrency_Models'First
            .. COM_Concurrency_Models'Last)
     of Boolean
   with Component_Size => 1, Size => DWORD'Size, Convention => C;

   type Thread_Message is record
      Window_Handle   : HWND;
      Raw_Message     : UINT;
      AdditionalW     : WPARAM;
      AdditionalL     : LPARAM;
      Posted_At       : DWORD;
      Cursor_Position : POINT;
      PrivateL        : DWORD;
   end record
   with Convention => C;

   type Window_Callback_Message is
     (UNKNOWN,
      CREATED,
      DESTROYING,
      MOVED,
      SIZE_CHANGED,
      ACTIVATION_CHANGED,
      KEYBOARD_FOCUS_CHANGED,
      LOSING_KEYBOARD_FOCUS,
      PAINT_REQUESTED,
      CLOSING,
      ERASE_BACKGROUND_REQUEST,
      VISIBILITY_CHANGED,
      ACTIVATION_CHANGED_EXTL,
      MOUSE_MOVED_CURSOR,
      SIZE_OVERRIDE_REQUEST,
      OBJECT_REQUEST,
      POSITION_CHANGING,
      POSITION_CHANGED,
      CONTEXT_MENU_REQUEST,
      ICON_REQUEST,
      CONFIRM_CREATION,
      AFTER_DESTROYING,
      CALCULATE_CLIENT_AREA_SIZE,
      ABSOLUTE_SCREEN_POSITION,
      FRAME_PAINT_REQUEST,
      ACTIVATION_CHANGED_INTL,
      UAH_DESTROY_WINDOW,
      MOUSE_MOVED_NON_CLIENT,
      LEFT_MOUSE_DOWN_NON_CLIENT,
      KEY_DOWN,
      KEY_UP,
      CHARACTER_DOWN,
      SYSTEM_KEY_DOWN,
      SYSTEM_COMMAND,
      CURSOR_MOVED,
      LEFT_MOUSE_DOWN_CLIENT,
      LEFT_MOUSE_UP_CLIENT,
      RIGHT_MOUSE_DOWN_CLIENT,
      RIGHT_MOUSE_UP_CLIENT,
      USER_RESIZING_WINDOW,
      LOSING_CURSOR_CAPTURE,
      USER_MOVING_WINDOW,
      STOPPED_MOVING_SIZING_WINDOW,
      IME_SET_CONTEXT,
      IME_CHANGED,
      MOUSE_LEFT_NON_CLIENT,
      NON_CLIENT_RENDER_POLICY_CHANGED)
   with Size => UINT'Size;

   for Window_Callback_Message use
     (UNKNOWN                          => 16#00_00#,
      CREATED                          => 16#00_01#,
      DESTROYING                       => 16#00_02#,
      MOVED                            => 16#00_03#,
      SIZE_CHANGED                     => 16#00_05#,
      ACTIVATION_CHANGED               => 16#00_06#,
      KEYBOARD_FOCUS_CHANGED           => 16#00_07#,
      LOSING_KEYBOARD_FOCUS            => 16#00_08#,
      PAINT_REQUESTED                  => 16#00_0F#,
      CLOSING                          => 16#00_10#,
      ERASE_BACKGROUND_REQUEST         => 16#00_14#,
      VISIBILITY_CHANGED               => 16#00_18#,
      ACTIVATION_CHANGED_EXTL          => 16#00_1C#,
      MOUSE_MOVED_CURSOR               => 16#00_20#,
      SIZE_OVERRIDE_REQUEST            => 16#00_24#,
      OBJECT_REQUEST                   => 16#00_3D#,
      POSITION_CHANGING                => 16#00_46#,
      POSITION_CHANGED                 => 16#00_47#,
      CONTEXT_MENU_REQUEST             => 16#00_7B#,
      ICON_REQUEST                     => 16#00_7F#,
      CONFIRM_CREATION                 => 16#00_81#,
      AFTER_DESTROYING                 => 16#00_82#,
      CALCULATE_CLIENT_AREA_SIZE       => 16#00_83#,
      ABSOLUTE_SCREEN_POSITION         => 16#00_84#,
      FRAME_PAINT_REQUEST              => 16#00_85#,
      ACTIVATION_CHANGED_INTL          => 16#00_86#,
      UAH_DESTROY_WINDOW               => 16#00_90#,
      MOUSE_MOVED_NON_CLIENT           => 16#00_A0#,
      LEFT_MOUSE_DOWN_NON_CLIENT       => 16#00_A1#,
      KEY_DOWN                         => 16#01_00#,
      KEY_UP                           => 16#01_01#,
      CHARACTER_DOWN                   => 16#01_02#,
      SYSTEM_KEY_DOWN                  => 16#01_04#,
      SYSTEM_COMMAND                   => 16#01_12#,
      CURSOR_MOVED                     => 16#02_00#,
      LEFT_MOUSE_DOWN_CLIENT           => 16#02_01#,
      LEFT_MOUSE_UP_CLIENT             => 16#02_02#,
      RIGHT_MOUSE_DOWN_CLIENT          => 16#02_04#,
      RIGHT_MOUSE_UP_CLIENT            => 16#02_05#,
      USER_RESIZING_WINDOW             => 16#02_14#,
      LOSING_CURSOR_CAPTURE            => 16#02_15#,
      USER_MOVING_WINDOW               => 16#02_16#,
      STOPPED_MOVING_SIZING_WINDOW     => 16#02_32#,
      IME_SET_CONTEXT                  => 16#02_81#,
      IME_CHANGED                      => 16#02_82#,
      MOUSE_LEFT_NON_CLIENT            => 16#02_A2#,
      NON_CLIENT_RENDER_POLICY_CHANGED => 16#03_1F#);

   type Window_Class is record
      Structure_Size     : unsigned := Window_Class'Size / char'Size;
      Class_Styles       : Window_Class_Styles_Flags := [others => False];
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
     (Extended_Style      : DWORD; --  In the future, figure out why Extended_Window_Class_Styles_Flags doesn't work
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

   function Handle_COM_Error (Result : HRESULT) return Boolean;

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
     (Message        : LPMSG;
      Window_Filter  : HWND := HWND (System.Null_Address);
      Filter_ID_Low  : UINT := 0;
      Filter_ID_High : UINT := 0) return BOOL
   with Import => True, External_Name => "GetMessageW", Convention => C;

   function Peek_Message_W
     (Message        : LPMSG;
      Window_Filter  : HWND := HWND (System.Null_Address);
      Filter_ID_Low  : UINT := 0;
      Filter_ID_High : UINT := 0;
      Parameters     : UINT := 1) return BOOL
   with Import => True, External_Name => "PeekMessageW", Convention => C;

   function Translate_Message (Message : LPMSG) return BOOL
   with Import => True, External_Name => "TranslateMessage", Convention => C;

   function Dispatch_Message_W (Message : LPMSG) return LRESULT
   with Import => True, External_Name => "DispatchMessageW", Convention => C;

   pragma Linker_Options ("-lole32");

   function COM_Initialize_Ex
     (Reserved : HANDLE := System.Null_Address; Concurrency_Model : DWORD)
      return HRESULT
   with Import => True, External_Name => "CoInitializeEx", Convention => C;

   procedure COM_Uninitialize
   with Import => True, External_Name => "CoUninitialize", Convention => C;

   task type COM_Loop_Task is
      entry Start;
   end COM_Loop_Task;

   task type Message_Loop_Task is
      entry Start
        (New_Class_Name, New_Window_Name : LPCSTR;
         New_Application_Handle          : HINSTANCE);
   end Message_Loop_Task;

   type Message_Loop_Task_Access is not null access Message_Loop_Task;

   type Windows_Window is new Window with record
      Message_Loop : Message_Loop_Task_Access;
   end record;

   type Windows_Window_Access is not null access Windows_Window;

   function Create_Windows_Window return Windows_Window_Access;
end AWTK.Windows;
