pragma Ada_2022;

with System.Storage_Elements;
with Ada.Text_IO;

package body AWTK.Windows is

   function Windows_Process_Callback
     (Window_Handle : HWND;
      Raw_Message   : UINT;
      AdditionalW   : WPARAM;
      AdditionalL   : LPARAM) return LRESULT
   is
      Message : Window_Callback_Message;
      discard : BOOL;
   begin
      begin
         Message := Window_Callback_Message'Enum_Val (Raw_Message);
      exception
         when Constraint_Error =>
            Message := UNKNOWN;
      end;

      if Message = UNKNOWN then
         Ada.Text_IO.Put_Line
           ("Callback fired! Message: "
            & Message'Image
            & ", value:"
            & Raw_Message'Image);
      end if;

      case Message is
         when others =>
            return
              Default_Process_Callback_A
                (Window_Handle, Raw_Message, AdditionalW, AdditionalL);
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

   function Handle_COM_Error (Result : HRESULT) return Boolean is
   begin
      Ada.Text_IO.Put_Line (Result'Image);
      if Result >= 16#80000000# then
         Ada.Text_IO.Put_Line ("COM Error:" & Result'Image);
         return True;
      else
         return False;
      end if;
   end Handle_COM_Error;

   task body COM_Loop_Task is
      Status : HRESULT;
   begin
      accept Start;
      Status := COM_Initialize_Ex (Concurrency_Model => 16#06#);
      if Handle_COM_Error (Status) then
         goto Stop;
      end if;

      <<Stop>>
      COM_Uninitialize;
   end COM_Loop_Task;

   task body Message_Loop_Task is
      Window_Handle      : HWND;
      Window_Class_Name  : LPCSTR;
      Window_Name        : LPCSTR;
      Application_Handle : HINSTANCE;
      COM_Task           : COM_Loop_Task_Access := new COM_Loop_Task;
   begin
      accept Start
        (New_Class_Name, New_Window_Name : LPCSTR;
         New_Application_Handle          : HINSTANCE)
      do
         Application_Handle := New_Application_Handle;
         Window_Class_Name := New_Class_Name;
         Window_Name := New_Window_Name;
      end Start;

      Window_Handle :=
        Create_Window_ExA
          (16#00000010#,
           Window_Class_Name,
           Window_Name,
           16#10cf0000#,
           0,
           0,
           500,
           500,
           Module_Handle => Application_Handle);
      if Window_Handle = HWND (System.Null_Address) then
         Ada.Text_IO.Put_Line
           ("Failed to create the window: " & Get_Last_Error_Formatted'Image);
         goto Stop;
      end if;

      COM_Task.Start;

      declare
         Message         : Thread_Message;
         pragma Volatile (Message);
         Message_Address : constant LPMSG := LPMSG (Message'Address);
         Continue        : BOOL := 1;

         discard_1 : BOOL;
         discard_2 : LRESULT;
      begin
         Ada.Text_IO.Put_Line ("Window Handle:" & Window_Handle'Image);
         Ada.Text_IO.Put_Line
           ("Starting message loop. (Message Address:"
            & Message_Address'Image
            & ", Size:"
            & Message'Size'Image
            & ")");
         loop
            Continue := Get_Message_W (Message_Address, Window_Handle);
            if Continue = -1 then
               Ada.Text_IO.Put_Line
                 ("Problem in message loop: "
                  & Get_Last_Error_Formatted'Image);
               exit;
            end if;
            discard_1 := Translate_Message (Message_Address);
            discard_2 := Dispatch_Message_W (Message_Address);
            exit when Continue = 0;
         end loop;
         Ada.Text_IO.Put_Line ("Message loop terminated.");
      end;
      <<Stop>>
   end Message_Loop_Task;

   ---------------------------
   -- Create_Windows_Window --
   ---------------------------

   function Create_Windows_Window return Windows_Window_Access is
      Application_Handle : constant HMODULE := Get_Module_Handle_A;
      Window_Class_Atom  : ATOM;
      Window_Name        : constant String := "Hello World" & ASCII.NUL;
      Window_Class_Name  : constant String := "Dummy" & ASCII.NUL;

      New_Window : constant Windows_Window_Access :=
        new Windows_Window'(Message_Loop => new Message_Loop_Task);
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
                "Failed to register the window class: "
                & Get_Last_Error_Formatted'Image;
         else
            Ada.Text_IO.Put_Line
              ("Registered class @" & Window_Class_Atom'Image);
         end if;
      end;

      Ada.Text_IO.Put_Line ("Starting window");
      New_Window.Message_Loop.Start
        (LPCSTR (Window_Class_Name'Address),
         LPCSTR (Window_Name'Address),
         Application_Handle);
      return New_Window;
   end Create_Windows_Window;

end AWTK.Windows;
