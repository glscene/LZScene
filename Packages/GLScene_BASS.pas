unit GLScene_BASS; 

interface

uses
  GLSMBASS, Bass, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMBASS', @GLSMBASS.Register); 
end; 

initialization
  RegisterPackage('GLScene_BASS', @Register); 
end.
