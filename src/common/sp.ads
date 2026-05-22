package SP
    with Preelaborate
is

   Version : constant String := "0.3.0";

   type User is (Human, Tool, Script);

   Current_User : User := Human;

end SP;
