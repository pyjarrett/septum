package SP
   with Preelaborate
is

    Version : constant String := "1.0.0-dev";

    type User is (Human, Script);

    Current_User : User := Human;

end SP;
