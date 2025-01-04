package AWTK is
   type Window is abstract tagged limited private;
   function Is_Fullscreen (Self : Window'Class) return Boolean;
   procedure Set_Fullscreen (Self : Window'Class; Status : Boolean);
   procedure Toggle_Fullscreen (Self : Window'Class);

   type Window_Access is not null access all Window'Class;

   function Create_Window return Window_Access;
private
   type Window is abstract tagged limited null record;
end AWTK;
