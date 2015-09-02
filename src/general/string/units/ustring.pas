unit uString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

//  TAnsiChars = array of AnsiChar;
  TChars = array of Char;

  TString = record
//    AnsiChars: TAnsiChars;
//    SString: shortstring;
    Chars: TChars;
//    WString: widestring;
   end;


operator := (aString: string): TString;
operator := (aString: TString): string;
//operator := (aString: ansistring): TString;
//operator := (aString: TString): ansistring;
//operator := (aString: shortstring): TString;
//operator := (aString: TString): shortstring;
//operator := (aString: widestring): TString;
//operator := (aString: TString): widestring;



implementation

operator:=(aString: string): TString;
begin
  SetLength(Result.Chars, Length(aString) + 1);
  System.Move(aString[1], Result.Chars[0], Length(aString));
end;

operator:=(aString: TString): string;
begin
  Result := String(aString.Chars);
end;

//operator:=(aString: ansistring): TString;
//begin
//  SetLength(Result.AnsiChars, Length(aString) + 1);
//  System.Move(aString[1], Result.AnsiChars[0], Length(aString));
//end;
//
//operator:=(aString: TString): ansistring;
//begin
//  Result := AnsiString(aString.AnsiChars);
//end;
//
//operator:=(aString: shortstring): TString;
//begin
//  Result.SString := aString;
//end;
//
//operator:=(aString: TString): shortstring;
//begin
//  Result := aString.SString;
//end;
//
//operator:=(aString: widestring): TString;
//begin
//  Result.WString := aString;
//end;
//
//operator:=(aString: TString): widestring;
//begin
//  Result := aString.WString;
//end;

end.

