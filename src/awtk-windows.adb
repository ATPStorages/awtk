pragma Ada_2022;

with Ada.Text_IO;

package body AWTK.Windows is

   procedure Window_Class_Styles_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Window_Class_Styles_Flags) is
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

   function Windows_Process_Callback (Window_Handle : HANDLE; Message : UINT; AdditionalW : WPARAM; AdditionL : LPARAM) return LRESULT is
   begin
      return 0;
   end;

   ---------------------------
   -- Create_Windows_Window --
   ---------------------------

   function Create_Windows_Window return Windows_Window is
      Application_Handle      : HANDLE := Get_Module_Handle_A (LPCSTR (System.Null_Address));
      Window_Class_Definition : Window_Class := (others => <>);
      Window_Class_Name       : Wide_String := "Dummy Window";
   begin
      Ada.Text_IO.Put_Line (Window_Class_Definition'Image);
      Window_Class_Definition.Window_Procedure := Windows_Process_Callback'Address;
      Window_Class_Definition.Window_Handle := Application_Handle;
      Window_Class_Definition.Window_Class_Name := LPCSTR (Window_Class_Name'Address);
      Ada.Text_IO.Put_Line (Register_Class_ExA (Window_Class_Definition'Address)'Image);
      Ada.Text_IO.Put_Line (Get_Last_Error'Image);
      
      return
        raise Program_Error
          with "Unimplemented function Create_Windows_Window";
   end Create_Windows_Window;

end AWTK.Windows;
