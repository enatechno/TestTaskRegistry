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
    procedure ChangeProgress(StepProgress: integer);
  public
    class var CurProgress: integer; // ������� �������� ��������� ������
    class var OnProgressChanged: TNotifyEvent; // ������� ��������� ���������
    class var TemplateStr: string; // ��������� ������ ��� ������

    /// ������������������� ����� �� ������� � �������� StartKeyName � ����
    /// ��������� (�� ��������� � ���������) ������ �������
    procedure SearchKeysByTemplate(const StartKeyName: string;
      // ��� ����, � ������� �������������� �����
      FoundKeyNames, // �������� ������, ��������������� �������� ������
      NotOpenedKeys: TStringList; // �������� ������, ������� �� ���� �������
      StepProgress: integer);
    // ����� �� ������ ��������� ������ ��� �������� ����
  end;

implementation

procedure TMyRegistry.SearchKeysByTemplate(const StartKeyName: string;
  FoundKeyNames, NotOpenedKeys: TStringList; StepProgress: integer);
var
  I: integer;
  Keys: TStringList;
  KeyName: string;
  MyReg: TMyRegistry;
begin
  MyReg := TMyRegistry.Create;
  try
    MyReg.RootKey := Self.RootKey;
    MyReg.Access := Self.Access;

    // �������� ��������� ����
    if MyReg.OpenKeyReadOnly(StartKeyName) then
      try
        Keys := TStringList.Create;
        try
          MyReg.GetKeyNames(Keys); // ��������� ������ ��������� ������
          if Keys.Count > 0 then
          begin
            StepProgress := Trunc(StepProgress / Keys.Count);
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
            MyReg.ChangeProgress(StepProgress);
        finally
          Keys.Free;
        end;
      finally
        MyReg.CloseKey;
      end
    else
    begin
      NotOpenedKeys.Add(StartKeyName);
      if StepProgress <> 100 then
        MyReg.ChangeProgress(StepProgress);
    end;

  finally
    MyReg.Free;
  end;
end;

procedure TMyRegistry.ChangeProgress(StepProgress: integer);
begin
  if StepProgress = 0 then
    Exit;

  CurProgress := CurProgress + StepProgress;

  if Assigned(OnProgressChanged) then
    OnProgressChanged(nil);
end;

end.
