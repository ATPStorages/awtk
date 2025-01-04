pragma Ada_2022;

with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with Ada.Text_IO;

package body AWTK.Windows is

   procedure Window_Class_Styles_Flags_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Window_Class_Styles_Flags)
   is
      Hit_First : Boolean := False;
   begin
      Output.Put ("[");
      for Flag in Value'First .. Value'Last loop
         if Value (Flag) then
            if Hit_First then
               Output.Put (", ");
            else
               Hit_First := True;
            end if;
            Output.Put (Flag'Image);
         end if;
      end loop;
      Output.Put ("]");
   end Window_Class_Styles_Flags_Put_Image;

   function Windows_Process_Callback
     (Window_Handle : HANDLE;
      Raw_Message   : UINT;
      AdditionalW   : WPARAM;
      AdditionL     : LPARAM) return LRESULT
   is
      function Raw_To_Message is new Ada.Unchecked_Conversion (UINT, Window_Callback_Message);
      Message : Window_Callback_Message := Raw_To_Message (Raw_Message);
   begin
      if not Message'Valid then
         Message := UNKNOWN;
      end if;
      Ada.Text_IO.Put_Line ("Callback fired! Message: " & Message'Image & ", value:" & Raw_Message'Image);
      return 0;
   end Windows_Process_Callback;

   function Get_Last_Error_Formatted return Wide_String is
      Last_Error     : constant DWORD := Get_Last_Error;
      Buffer_Pointer : System.Storage_Elements.Integer_Address;
      Buffer_Size    : DWORD;
      pragma Volatile (Buffer_Pointer);
   begin
      Buffer_Size :=
        Format_Message_W
          (Parameters  => 16#1100#,
           Mesasge_ID  => Last_Error,
           Language_ID => 0,
           Buffer      => LPTSTR (Buffer_Pointer'Address),
           Buffer_Size => 1);
      if Buffer_Size = 0 then
         return
           "Last error code:"
           & Last_Error'Wide_Image
           & ", additionally FormatMessageW failed with"
           & Get_Last_Error'Wide_Image;
      else
         declare
            Error_String : Wide_String (1 .. Integer (Buffer_Size) - 2);
            for Error_String'Address use
              System.Storage_Elements.To_Address (Buffer_Pointer);
         begin
            return
              "Last error:"
              & Last_Error'Wide_Image
              & " ["
              & Error_String
              & "]";
         end;
      end if;
   end Get_Last_Error_Formatted;

   ---------------------------
   -- Create_Windows_Window --
   ---------------------------

   function Create_Windows_Window return Windows_Window is
      Application_Handle : constant HMODULE := Get_Module_Handle_A;
      Window_Class_Atom  : ATOM;
      Window_Handle      : HWND;
      Window_Name        : constant Wide_String :=
        "Hello World" & ASCII.NUL'Wide_Image;
      Window_Class_Name  : constant Wide_String :=
        "Dummy" & ASCII.NUL'Wide_Image;
   begin
      declare
         Window_Class_Definition : constant Window_Class :=
           (Window_Procedure  => Windows_Process_Callback'Address,
            Window_Handle     => Application_Handle,
            Window_Class_Name => LPCSTR (Window_Class_Name'Address),
            others            => <>);
      begin
         Ada.Text_IO.Put_Line
           ("Registering class @"
            & Window_Class_Definition'Address'Image
            & ": "
            & Window_Class_Definition'Image);
         Window_Class_Atom :=
           Register_Class_ExA (Window_Class_Definition'Address);
         if Window_Class_Atom = 0 then
            raise Program_Error
              with
                "Failed to initialize the window class. "
                & Get_Last_Error_Formatted'Image;
         end if;
      end;

      Window_Handle :=
        Create_Window_ExA
          (0,
           LPCSTR (Window_Class_Name'Address),
           LPCSTR (Window_Name'Address),
           16#10cf0000#,
           0,
           0,
           500,
           500,
           Module_Handle => Application_Handle);
      if Window_Handle = HWND (System.Null_Address) then
         raise Program_Error
           with
             "Failed to create the window. "
             & Get_Last_Error_Formatted'Image
             & ", created class ATOM:"
             & Window_Class_Atom'Image;
      end if;
      Ada.Text_IO.Put_Line ("Window Handle:" & Window_Handle'Image);

      return
        raise Program_Error
          with "Unimplemented function Create_Windows_Window";
   end Create_Windows_Window;

end AWTK.Windows;
