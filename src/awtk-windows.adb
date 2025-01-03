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

   ---------------------------
   -- Create_Windows_Window --
   ---------------------------

   function Create_Windows_Window return Windows_Window is
   begin
      Ada.Text_IO.Put_Line (Register_Class_ExA (System.Null_Address)'Image);
      Ada.Text_IO.Put_Line (Get_Last_Error'Image);
      
      return
        raise Program_Error
          with "Unimplemented function Create_Windows_Window";
   end Create_Windows_Window;

end AWTK.Windows;
