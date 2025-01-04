with AWTK.Windows;
separate (AWTK) function Create_Window return Window_Access is
begin
   return AWTK.Windows.Create_Windows_Window;
end Create_Window;