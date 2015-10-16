unit uSerialForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, uModbus, uConfiguratorData, uLibrary;

type


  (*

  #define MAX_LENGHT_UID	64
  #define MAX_LENGHT_SN		32
  #define MAX_LENGHT_DATA	32
  typedef __packed struct{
  	char UID[MAX_LENGHT_UID]; //UID - строка 256 символов с 0 в конце
  	char SN[MAX_LENGHT_SN]; 	 // серийный номер - строка с нулем в конце
  	char DATA[MAX_LENGHT_DATA]; // дата создания модуля
  }T_UID_STRUCT;

  *)
  TSerialRecord = record
  	Uid: array[0 .. 63] of AnsiChar;
    Sn: array[0 .. 31] of AnsiChar;
    CreDate: array[0 .. 31] of AnsiChar;
  end;

  { TSerialForm }

  TSerialForm = class(TDetailedForm)
    CancelButton: TButton;
    UidEditButton: TEditButton;
    Label1: TLabel;
    OkButton: TButton;
    DateEdit: TDateEdit;
    DateLabel: TLabel;
    SerialLabeledEdit: TLabeledEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure UidEditButtonButtonClick(Sender: TObject);
  private
    fDeviceData: TDeviceData;
    fModbusData: TModbusData;
  public
    procedure Load(const aContentData: array of TContentData); override;
    procedure Unload; override;
    { public declarations }
  end;




implementation

{$R *.lfm}

{ TSerialForm }

procedure TSerialForm.OkButtonClick(Sender: TObject);
var
  Frame: IFrame;
  SR: TSerialRecord;
  Buffer: TBuffer;
  T: string;
begin
  SR.Sn := SerialLabeledEdit.Text;
  SR.Uid := UidEditButton.Text;
	SR.CreDate := DateEdit.Text;
  Buffer[0] := 71;
  Buffer[1] := 1;
  System.Move(SR.Uid[0], Buffer[2], 64);
  System.Move(SR.Sn[0], Buffer[2 + 64], 32);
  System.Move(SR.CreDate[0], Buffer[2 + 64 + 32], 32);

  Frame := uModBus.WriteStruct(fDeviceData.SlaveId, Buffer, 130, 1000);
  T := Frame.ToString;
  fModbusData.Controller.InQueue(Frame);
  Frame.Responded;
  Close;
end;

procedure TSerialForm.UidEditButtonButtonClick(Sender: TObject);
begin
	UidEditButton.Text := uLibrary.GetNewGuid;
end;

procedure TSerialForm.CancelButtonClick(Sender: TObject);
begin
 Close;
end;

procedure Serialize(const ASerialRecord: TSerialRecord; out APdu: TPdu);
begin

end;

procedure Deserialize(const APdu: TPdu; out ASerialRecord: TSerialRecord);
begin
  System.Move(APdu[2], ASerialRecord.Uid[0], 64);
  System.Move(APdu[2 + 64], ASerialRecord.Sn[0], 32);
  System.Move(APdu[2 + 64 + 32], ASerialRecord.CreDate[0], 32);
end;

procedure TSerialForm.Load(const aContentData: array of TContentData);
var
  Frame: IFrame;
  Respond: TPdu;
  T: String;
  SR: TSerialRecord;
begin
  if aContentData[0] is TDeviceData then
    fDeviceData := TDeviceData(aContentData[0]);
  if aContentData[1] is TModbusData then
    fModbusData := TModbusData(aContentData[1]);

  // Request
  Frame := uModbus.ReadSerial(fDeviceData.SlaveId, 1000);
  Frame.Priority:= TPriority.prHigh;
  fModbusData.Controller.InQueue(Frame);
 sleep(100);
  if not Frame.Responded then
  begin
    Respond := Frame.ResponsePdu^;
    DeSerialize(Frame.ResponsePdu^, SR);
    if (SR.Sn[0] = '?') or (SR.Uid[0] = '?') or (SR.CreDate[0] = '?') then
    begin
    	SerialLabeledEdit.ReadOnly:= False;
      UidEditButton.ReadOnly:= False;
      DateEdit.ReadOnly:= False;
      OkButton.Visible:=True;
    end
    else
    begin
    	SerialLabeledEdit.ReadOnly:= True;
      UidEditButton.ReadOnly:= True;
      DateEdit.ReadOnly:= True;
      OkButton.Visible:=False;
    end;
    SerialLabeledEdit.Text := SR.Sn;
   	UidEditButton.Text := SR.Uid;
    DateEdit.Text:= SR.CreDate;
  end;


end;

procedure TSerialForm.Unload;
begin

end;

end.

