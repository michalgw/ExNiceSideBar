{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NiceSidebarLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  NiceSideBar, NiceSideBarReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NiceSideBarReg', @NiceSideBarReg.Register);
end;

initialization
  RegisterPackage('NiceSidebarLaz', @Register);
end.
