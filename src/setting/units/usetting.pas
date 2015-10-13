unit uSetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { ISetting }

  ISetting = interface
  ['{0EA92F37-F025-4F19-B1F5-4EB64DF4E8C8}']

    function GetBackupTimeout: dword;
    function GetDeveloperImage: String;
    function GetDeveloperLibrary: String;
    function GetKeepAliveTimeout: dword;
    function GetKeepAliveAddress: Byte;
    function GetRepeats: Byte;
    function GetResponseTimeout: dword;
    function GetTimeout: dword;
    function GetUserImage: String;
    function GetUserLibrary: String;
    procedure SetBackupTimeout(const aBackupTimeout: dword);
    procedure SetDeveloperImage(const aDeveloperImage: String);
    procedure SetDeveloperLibrary(const aDeveloperLibrary: String);
    procedure SetKeepAliveAddress(const aKeepAliveAddress: Byte);
    procedure SetKeepAliveTimeout(const aKeepAliveTimeout: dword);
    procedure SetRepeats(const aRepeats: Byte);
    procedure SetResponseTimeout(const aResponseTimeout: dword);
    procedure SetTimeout(const aTimeout: dword);
    procedure SetUserImage(const aUserImage: String);
    procedure SetUserLibrary(const aUserLibrary: String);
    function GetBaudRate: dword;
    procedure SetBaudRate(const aBaudRate: dword);
    function GetParity: Byte;
    procedure SetParity(const aParity: Byte);
    function GetByteSize: Byte;
    procedure SetByteSize(const aByteSize: Byte);
    function GetStopBits: Byte;
    procedure SetStopBits(const aStopBits: Byte);
    function GetThreeAndHalf: Byte;
    procedure SetThreeAndHalf(const aThreeAndHalf: Byte);

    // Ip
    function GetIp: String;
    procedure SetIp(const aIp: String);

    // Порт
    function GetPort: Word;
    procedure SetPort(const aPort: Word);

    function GetServerPort: Word;
    procedure SetServerPort(const aPort: Word);

    function GetUpdateEveryStart: Boolean;
    function GetUpdateEveryWeek: Boolean;

  	procedure SetUpdateEveryWeek(AValue: Boolean);
    procedure SetUpdateEveryStart(AValue: Boolean);

    property BaudRate: dword read GetBaudRate write SetBaudRate;
    property ByteSize: Byte read GetByteSize write SetByteSize;
    property Parity: Byte read GetParity write SetParity;
    property StopBits: Byte read GetStopBits write SetStopBits;
    property ThreeAndHalf: Byte read GetThreeAndHalf write SetThreeAndHalf;
    property Ip: String read GetIp write SetIp;
    property Port: Word read GetPort write SetPort;
    property ServerPort: Word read GetServerPort write SetServerPort;
    property KeepAliveAddress: Byte read GetKeepAliveAddress write SetKeepAliveAddress;
    property KeepAliveTimeout: dword read GetKeepAliveTimeout write SetKeepAliveTimeout;
    property Timeout: dword read GetTimeout write SetTimeout;
    property ResponseTimeout: dword read GetResponseTimeout write SetResponseTimeout;
    property BackupTimeout: dword read GetBackupTimeout write SetBackupTimeout;
    property Repeats: Byte read GetRepeats write SetRepeats;
    property DeveloperLibrary: String read GetDeveloperLibrary
      write SetDeveloperLibrary;
    property DeveloperImage: String read GetDeveloperImage write SetDeveloperImage;
    property UserLibrary: String read GetUserLibrary write SetUserLibrary;
    property UserImage: String read GetUserImage write SetUserImage;
    property UpdateEveryStart: Boolean read GetUpdateEveryStart write SetUpdateEveryStart;
    property UpdateEveryWeek: Boolean read GetUpdateEveryWeek write SetUpdateEveryWeek;
    function GetUserData: string;
    property UserData: string read GetUserData;
  end;

  function GetSetting(const aFileName: string = ''): ISetting; external 'setting.dll';

implementation

end.

