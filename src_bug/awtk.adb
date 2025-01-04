with AWTK.Windows;
with GNAT.OS_Lib;

package body AWTK is

   -------------------
   -- Is_Fullscreen --
   -------------------

   function Is_Fullscreen (Self : Window'Class) return Boolean is
   begin
      pragma
        Compile_Time_Warning (Standard.True, "Is_Fullscreen unimplemented");
      return raise Program_Error with "Unimplemented function Is_Fullscreen";
   end Is_Fullscreen;

   --------------------
   -- Set_Fullscreen --
   --------------------

   procedure Set_Fullscreen (Self : Window'Class; Status : Boolean) is
   begin
      pragma
        Compile_Time_Warning (Standard.True, "Set_Fullscreen unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Fullscreen";
   end Set_Fullscreen;

   -----------------------
   -- Toggle_Fullscreen --
   -----------------------

   procedure Toggle_Fullscreen (Self : Window'Class) is
   begin
      pragma
        Compile_Time_Warning
          (Standard.True, "Toggle_Fullscreen unimplemented");
      raise Program_Error with "Unimplemented procedure Toggle_Fullscreen";
   end Toggle_Fullscreen;

   -------------------
   -- Create_Window --
   -------------------

   function Create_Window return Window'Class is
   begin
      case GNAT.OS_Lib.Directory_Separator is
         when '/' =>
            return AWTK.Windows.Create_Windows_Window;

         when '\' =>
            return AWTK.Windows.Create_Windows_Window;

         when others =>
            null;
      end case;
      return raise Program_Error with "AWTK is not supported on this platform";
   end Create_Window;

end AWTK;
