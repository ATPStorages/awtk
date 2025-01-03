pragma Ada_2012;
package body AWTK.Windows is

   ---------------------------
   -- Create_Windows_Window --
   ---------------------------

   function Create_Windows_Window return Windows_Window is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Windows_Window unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Create_Windows_Window";
   end Create_Windows_Window;

end AWTK.Windows;
