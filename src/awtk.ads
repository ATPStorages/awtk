package AWTK is
   type Window is limited private;
   function Is_Fullscreen (Self : Window) return Boolean;
   procedure Set_Fullscreen (Self : Window; Status : Boolean);
   procedure Toggle_Fullscreen (Self : Window);
   function Create_Window return Window;
private
   type Window is limited null record;
end AWTK;
