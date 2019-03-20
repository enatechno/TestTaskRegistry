program TestTaskRegistry;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  MyRegistry in 'MyRegistry.pas';

type
  TMyTestTask = class // �����, ����������� �������� ������� ���������
  const
    RESULT_FILE_NAME = 'result.txt';
  private
    FMyRegistry: TMyRegistry;
    FFoundKeyNames: TStringList;
    FNotOpenedKeyNames: TStringList;

    FStartKeyName: string;

    class function Is64BitWindows: Boolean; static;
    /// <summary>
    /// ������������� ����������� � ������� ������ ��������� ������
    /// </summary>
    procedure InitProgress;

    /// <summary>
    /// ������ ����������� ������ � ���� result.txt, ������������� ����� � .exe
    /// </summary>
    procedure SaveSearchResult;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// ��������� �������� ������ �� ������������
    /// </summary>
    procedure GetSourceData;

    /// <summary>
    /// ����� ������ � ����������� ����������� ������ � �������
    /// </summary>
    procedure SearchKeyNames;

    /// <summary>
    /// ����������� �������� ��������� � ������� ��� ������
    /// </summary>
    procedure ShowProgress(Sender: TObject);
  end;

constructor TMyTestTask.Create;
begin
  inherited;

  FMyRegistry := TMyRegistry.Create;

  FMyRegistry.RootKey := HKEY_LOCAL_MACHINE; // ������ HKLM

  // � ���������� �� ����������� �������, ������������ ��� �������
  if TMyTestTask.Is64BitWindows then
    FMyRegistry.Access := KEY_READ or KEY_WOW64_64KEY
  else
    FMyRegistry.Access := KEY_READ;

  FFoundKeyNames := TStringList.Create;
  FNotOpenedKeyNames := TStringList.Create;
end;

destructor TMyTestTask.Destroy;
begin
  FFoundKeyNames.Free;
  FNotOpenedKeyNames.Free;
  FMyRegistry.Free;

  inherited;
end;

/// �������� ����������� Windows (����� � gunsmoker.ru)
class function TMyTestTask.Is64BitWindows: Boolean;
var
  IsWow64Process: function(hProcess: THandle; out Wow64Process: Bool)
    : Bool; stdcall;
  Wow64Process: Bool;
begin
{$IF Defined(CPU64)}
  Result := true; // 64-������ ��������� ����������� ������ �� Win64
{$ELSEIF Defined(CPU16)}
  Result := false; // Win64 �� ������������ 16-��������� ����������
{$ELSE}
  // 32-������ ��������� ����� �������� � �� 32-��������� � �� 64-��������� Windows
  // ��� ��� ���� ������ ������� ����������� ������������
  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');

  Wow64Process := false;
  if Assigned(IsWow64Process) then
    Wow64Process := IsWow64Process(GetCurrentProcess, Wow64Process) and
      Wow64Process;

  Result := Wow64Process;
{$ENDIF}
end;

procedure TMyTestTask.InitProgress;
begin
  FMyRegistry.CurProgress := 0;
  Write('�����:   0%');
  FMyRegistry.OnProgressChanged := ShowProgress;
end;

procedure TMyTestTask.ShowProgress(Sender: TObject);
begin
  if FMyRegistry.CurProgress > 100 then
    Exit;
  Write(#8#8#8#8, FMyRegistry.CurProgress:3, '%');
end;

procedure TMyTestTask.GetSourceData;
begin
  Writeln('������� ��������� ���� ��� ������ (��� �������� �������� ������) � ������� Enter:');
  Readln(FStartKeyName);
  Writeln('������� ��������� ��� ������ � ����� ����� (��� �������� �������� ������) � ������� Enter:');
  Readln(FMyRegistry.TemplateStr);
end;


procedure TMyTestTask.SearchKeyNames;
var
  i: integer;
begin
  InitProgress;

  try
    FMyRegistry.SearchKeysByTemplate(FStartKeyName, FFoundKeyNames,
      FNotOpenedKeyNames, 100);
  except
    on E: Exception do
      Writeln(#13 + '������ ������ � �������: ', E.Message);
  end;

  if FNotOpenedKeyNames.Count > 0 then
    for i := 0 to FNotOpenedKeyNames.Count - 1 do
      Writeln(Format(#13 + '������ ������ ����� %s', [FNotOpenedKeyNames[i]]));

  if FFoundKeyNames.Count > 0 then
  begin
    SaveSearchResult;
    Writeln(Format(#13 + '������� ������: %d. ���������� ������ � ����� %s',
      [FFoundKeyNames.Count, RESULT_FILE_NAME]));
  end
  else
    Writeln(#13 + '����� �� ������� �� �������.');
end;


procedure TMyTestTask.SaveSearchResult;
begin
  try
    FFoundKeyNames.SaveToFile(RESULT_FILE_NAME);
  except
    on E: Exception do
      Writeln('������ ������ ����������� � ����: ', E.Message);
  end;
end;

///
var
  MyTestTask: TMyTestTask;

begin
  //������������ ������� �� ������� �������� CP1251 (Windows-1251)
  SetConsoleCP(1251);
  SetConsoleOutputCP(1251);

  try
    MyTestTask := TMyTestTask.Create;
    try
      MyTestTask.GetSourceData;
      MyTestTask.SearchKeyNames;
    finally
      MyTestTask.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Writeln('������� Enter ��� ������...');
  Readln;

end.
