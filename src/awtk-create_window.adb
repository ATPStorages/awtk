separate (AWTK)
function Create_Window return Window_Access is
begin
   return raise Program_Error with "AWTK is not supported on this platform";
end Create_Window;
