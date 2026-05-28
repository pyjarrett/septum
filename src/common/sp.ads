package SP
    with Preelaborate
is

   Version : constant String := "0.4.0-dev";

   type User is (Human, Tool, Script);

   Current_User : User := Human;

end SP;
