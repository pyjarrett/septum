with SP.Memory_Tests;
with SP.Strings.Tests;

with Trendy_Test.Reports;

procedure Septum_Tests is
begin
    Trendy_Test.Register (SP.Memory_Tests.All_Tests);
    Trendy_Test.Register (SP.Strings.Tests.All_Tests);

    Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run);
end Septum_Tests;
