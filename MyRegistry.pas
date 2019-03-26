unit MyRegistry;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Win.Registry;

type
  TMyRegistry = class(TRegistry)
  private
    procedure ChangeProgress(StepProgress: Extended);
    function Clone: TMyRegistry;
  public
    class var CurProgress: Extended; // ������� �������� ��������� ������
    class var ProgressValue: integer;  // ����������� �������� ���������
    class var OnProgressChanged: TNotifyEvent; // ������� ��������� ���������
    class var TemplateStr: string; // ��������� ������ ��� ������

    procedure InitProgress;

    /// <summary>
    /// ������������������� ����� �� ������� � �������� StartKeyName � ����
    /// ��������� (�� ��������� � ���������) ������ �������
    /// </summary>
    /// <param name='StartKeyName'>��� ����, � ������� �������������� �����</param>
    /// <param name='FoundKeyNames'>������ ��������� ������, ��������������� �������� ������</param>
    /// <param name='NotOpenedKeys'>������ ������, ������� �� ���� �������</param>
    /// <param name='StepProgress'>����� (� %) �� ������ ��������� ������ ��� �������� ����</param>
    procedure SearchKeysByTemplate(const StartKeyName: string;
      FoundKeyNames, NotOpenedKeys: TStringList; StepProgress: Extended);

  end;

implementation

procedure TMyRegistry.InitProgress;
begin
  CurProgress := 0;
  ProgressValue := 0;
end;

function TMyRegistry.Clone: TMyRegistry;
begin
  Result := TMyRegistry.Create;
  Result.RootKey := Self.RootKey;
  Result.Access := Self.Access;
end;

procedure TMyRegistry.SearchKeysByTemplate(const StartKeyName: string;
  FoundKeyNames, NotOpenedKeys: TStringList; StepProgress: Extended);
var
  I: integer;
  Keys: TStringList;
  KeyName: string;
  MyReg: TMyRegistry;
  ErrorStr: string;
begin
  MyReg := Self.Clone;
  try
    // �������� ��������� ����
    if MyReg.OpenKeyReadOnly(StartKeyName) then
      try
        Keys := TStringList.Create;
        try
          MyReg.GetKeyNames(Keys); // ��������� ������ ��������� ������
          if Keys.Count > 0 then
          begin
            StepProgress := StepProgress / Keys.Count;
            for I := 0 to Keys.Count - 1 do
            begin
              // ��������� ������� ����� �����
              KeyName := Format('%s\%s', [StartKeyName, Keys[I]]);

              // �������� ������� ��������� ��������� � ����� ����� �� ������
              if (TemplateStr = '') or // (��� ����� ��������)
                (Pos(UpperCase(TemplateStr), UpperCase(Keys[I])) > 0) then
              begin
                FoundKeyNames.Add(KeyName);
              end;

              // ����������� ����� - ����� �� ��������� ������
              MyReg.SearchKeysByTemplate(KeyName, FoundKeyNames, NotOpenedKeys,
                StepProgress);
            end;
          end
          else
            // ���� ���� �� ����� ��������� ������, �� ������ � ��� ��������� �
            // ����� ����������� ��� ��������� ����������� � �����
            MyReg.ChangeProgress(StepProgress);
        finally
          FreeAndNil(Keys);
        end;
      finally
        MyReg.CloseKey;
      end
    else
    begin
      //ErrorStr := SysErrorMessage(GetLastError);
      NotOpenedKeys.Add(StartKeyName);

      // �� ����������� ���� - ��� ���� ��������
      // (�� ����������� ����� �� �������� ����� ������ ���� � �������� StepProgress = 100)
      if StepProgress < 100 then
        MyReg.ChangeProgress(StepProgress);
    end;
  finally
    FreeAndNil(MyReg);
  end;
end;

procedure TMyRegistry.ChangeProgress(StepProgress: Extended);
var
  i: integer;
begin
  CurProgress := CurProgress + StepProgress;
  i := Trunc(CurProgress);

  if ProgressValue = i then  // ���������� �� ����������� �������� ���������
    Exit;

  ProgressValue := i;

  if Assigned(OnProgressChanged) then
    OnProgressChanged(nil);
end;

end.
