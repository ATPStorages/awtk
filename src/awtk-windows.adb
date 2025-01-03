pragma Ada_2022;

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
      Message       : UINT;
      AdditionalW   : WPARAM;
      AdditionL     : LPARAM) return LRESULT is
   begin
      return 0;
   end Windows_Process_Callback;

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
         Ada.Text_IO.Put_Line ("Registering class @" & Window_Class_Definition'Address'Image &": " & Window_Class_Definition'Image);
         Window_Class_Atom :=
           Register_Class_ExA (Window_Class_Definition'Address);
         if Window_Class_Atom = 0 then
            raise Program_Error
              with
                "Failed to initialize the window class. Last error code:"
                & Get_Last_Error'Image;
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
      Ada.Text_IO.Put_Line ("Window Handle:" & Window_Handle'Image);
      if Window_Handle = HWND (System.Null_Address) then
         raise Program_Error
           with
             "Failed to create the window. Last error code:"
             & Get_Last_Error'Image
             & ", created class ATOM:"
             & Window_Class_Atom'Image;
      end if;
      Ada.Text_IO.Put_Line (Window_Handle'Image);

      return
        raise Program_Error
          with "Unimplemented function Create_Windows_Window";
   end Create_Windows_Window;

end AWTK.Windows;
