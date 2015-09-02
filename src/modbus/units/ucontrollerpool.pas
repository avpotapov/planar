unit uControllerPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WinSock2,
  uBase, uModbus;

type
  TTsControllersSpec = specialize TThreadSafeWrapper<TControllers>;

  { TTsControllers }

  TTsControllers = class(TTsControllersSpec)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  { TControllerPool }

  TControllerPool = class(TBase, IControllerPool)
  private
    fControllers: TTsControllers;
    fOnChangePool: TChangePoolEvent;
  public
    constructor Create;
    destructor Destroy; override;

  public
    // только для серверного сокета
    function ReplaceConnection(const aClientSocket: TSocket): boolean;
    procedure Add(aController: IController);
    procedure Remove(aController: IController);
    procedure SetOnChangePool(const aOnChangePool: TChangePoolEvent);
  end;


implementation

uses uCommunication, uConnection;


{ TTsControllers }

constructor TTsControllers.Create;
begin
  inherited Create;
  fT := TControllers.Create;
end;

destructor TTsControllers.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fT)
end;

{ TControllerPool }

constructor TControllerPool.Create;
begin
  inherited Create;
   fControllers := TTsControllers.Create;
end;

destructor TControllerPool.Destroy;
begin
   FreeAndNil(fControllers);
   inherited Destroy;
end;

function TControllerPool.ReplaceConnection(const aClientSocket: TSocket
  ): boolean;
var
  Controllers: TControllers;
  Controller: IController;
begin
  Result := False;
  Controllers := fControllers.Lock;
  try
    for Controller in Controllers do

      if Controller.TypeController = tTypeController.mbAcTcp then
      begin
        Controller.Close;
        Controller.Transaction.Communication := TTcpCommunication.Create;
        (Controller.Transaction.Communication as ITcpCommunication).TcpConnection :=TAcTcpConnection.Create(aClientSocket);
        Controller.Open;
        Result := True;
        Break;
      end;

  finally
    fControllers.Unlock;
  end;
end;

procedure TControllerPool.Add(aController: IController);
var
  Controllers: TControllers;
begin
  Controllers := fControllers.Lock;
  try
    // Find
    if Controllers.IndexOf(aController) > -1 then
      Exit;

    Controllers.Add(aController);

    if Assigned(fOnChangePool) then
      fOnChangePool(aController, TTypeListAction.laAdded);

  finally
    fControllers.Unlock;
  end;

end;

procedure TControllerPool.Remove(aController: IController);
var
  Controllers: TControllers;
begin
  Controllers := fControllers.Lock;
  try
    // Find
    if Controllers.IndexOf(aController) < 0 then
      Exit;

    if Assigned(fOnChangePool) then
      fOnChangePool(aController, TTypeListAction.laRemoved);
    Controllers.Remove(aController);
  finally
    fControllers.Unlock;
  end;


end;

procedure TControllerPool.SetOnChangePool(const aOnChangePool: TChangePoolEvent
  );
begin
  fOnChangePool := aOnChangePool;
end;



end.

