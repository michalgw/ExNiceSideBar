{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExNiceSidebarLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  ExNiceSideBar, ExNiceSideBarReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ExNiceSideBarReg', @ExNiceSideBarReg.Register);
end;

initialization
  RegisterPackage('ExNiceSidebarLaz', @Register);
end.
