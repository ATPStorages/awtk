pragma Ada_2022;

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
     (Window_Handle : HWND;
      Raw_Message   : UINT;
      AdditionalW   : WPARAM;
      AdditionalL   : LPARAM) return LRESULT
   is
      Message : Window_Callback_Message;
   begin
      begin
         Message := Window_Callback_Message'Enum_Val (Raw_Message);
      exception
         when Constraint_Error =>
            Message := UNKNOWN;
      end;
      Ada.Text_IO.Put_Line
        ("Callback fired! Message: "
         & Message'Image
         & ", value:"
         & Raw_Message'Image);
      case Message is
         when others =>
            return Default_Process_Callback_A (Window_Handle, Raw_Message, AdditionalW, AdditionalL);
      end case;
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
      Window_Name        : constant String :=
        "Hello World" & ASCII.NUL;
      Window_Class_Name  : constant String :=
        "Dummy" & ASCII.NUL;
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

      declare
         Message         : Thread_Message;
         pragma Volatile (Message);
         Message_Address : LPMSG := LPMSG (Message'Address);
         Continue        : BOOL := 1;

         discard_1 : BOOL;
         discard_2 : LRESULT;
      begin
         Ada.Text_IO.Put_Line ("Window Handle:" & Window_Handle'Image);
         Ada.Text_IO.Put_Line ("Starting message loop. (Message Address:" & Message_Address'Image & ", Size:" & Message'Size'Image & ")");
            loop
               Continue := Get_Message_W (Message_Address, Window_Handle);
               discard_1 := Translate_Message (Message_Address);
               discard_2 := Dispatch_Message_W (Message_Address);
               Ada.Text_IO.Put_Line (Continue'Image);
               exit when Continue = 0;
            end loop;
            Ada.Text_IO.Put_Line ("Message loop terminated.");
      end;

      return (others => <>);
   end Create_Windows_Window;

end AWTK.Windows;
