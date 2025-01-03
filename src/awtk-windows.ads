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
     (REDRAW_ON_CLIENT_AREA_CHANGE_VERTICALLY    => 2**0,
      REDRAW_ON_CLIENT_AREA_CHANGE_HORIZONTALLY  => 2**1,
      UNDEFINED_1                                => 2**2,
      RECEIVE_DOUBLE_CLICK_MESSAGES              => 2**3,
      UNDEFINED_2                                => 2**4,
      ALLOCATE_DEVICE_CONTEXT_FOR_EACH_WINDOW    => 2**5,
      ALLOCATE_SHARED_DEVICE_CONTEXT             => 2**6,
      ALLOCATE_PARENT_CLIPPING_DEVICE_CONTEXT    => 2**7,
      UNDEFINED_3                                => 2**8,
      DISABLE_CLOSE_BUTTON                       => 2**9,
      UNDEFINED_4                                => 2**10,
      SAVE_OCCLUDED_SCREEN_AS_BITMAP             => 2**11,
      ALIGN_CLIENT_AREA_ON_BOUNDARY_HORIZONTALLY => 2**12,
      ALIGN_WINDOW_ON_BOUNDARY_HORIZONTALLY      => 2**13,
      APPLICATION_GLOBAL_CLASS                   => 2**14,
      UNDEFINED_5                                => 2**15);

   type Window_Class_Styles_Flags is
     array (Window_Class_Styles'First .. Window_Class_Styles'Last)
     of Boolean with
     Put_Image => Window_Class_Styles_Flags_Put_Image, Component_Size => 1, Size => unsigned'Size;
   procedure Window_Class_Styles_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Window_Class_Styles_Flags);

   type Window_Class is record
      Structure_Size    : unsigned := Window_Class'Size / char'Size;
      Class_Styles      : Window_Class_Styles_Flags;
      Window_Procedure  : System.Address;
      Class_Size_Extra  : int := 0;
      Window_Size_Extra : int := 0;
      Window_Handle     : System.Address;
      Icon_Handle       : System.Address;
      Cursor_Handle     : System.Address;
      Brush_Handle      : System.Address;
      Resource_Name     : System.Address;
      Window_Class_Name : System.Address;
      Small_Icon_Handle : System.Address;
   end record
      with Convention => C;

   function Register_Class_ExA (Class_Pointer : System.Address) return unsigned_short
      with Import => True, External_Name => "RegisterClassExA", Convention => C;

   type Windows_Window is new Window with null record;
   function Create_Windows_Window return Windows_Window;
end AWTK.Windows;
