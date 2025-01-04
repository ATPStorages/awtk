with AWTK.Windows;
separate (AWTK) function Create_Window return not null access Window'Class is
begin
   return AWTK.Windows.Create_Windows_Window;
end Create_Window;