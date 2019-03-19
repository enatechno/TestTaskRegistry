program TestTaskRegistry;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  MyRegistry in 'MyRegistry.pas';

type
  TMyTestTask = class // класс, выполняющий основные функции программы
  const
    RESULT_FILE_NAME = 'result.txt';
  private
    FMyRegistry: TMyRegistry;
    FFoundKeyNames: TStringList;
    FNotOpenedKeyNames: TStringList;

    FStartKeyName: string;

    class function Is64BitWindows: Boolean; static;
    procedure InitProgress;
    procedure SaveSearchResult;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetSourceData;
    procedure SearchKeyNames;
    procedure ShowProgress(Sender: TObject);
  end;

constructor TMyTestTask.Create;
begin
  inherited;

  FMyRegistry := TMyRegistry.Create;

  FMyRegistry.RootKey := HKEY_LOCAL_MACHINE; // корень HKLM

  // в зависимоти от разрядности системы, определяется тип доступа
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

/// проверка разрядности Windows (взято с gunsmoker.ru)
class function TMyTestTask.Is64BitWindows: Boolean;
var
  IsWow64Process: function(hProcess: THandle; out Wow64Process: Bool)
    : Bool; stdcall;
  Wow64Process: Bool;
begin
{$IF Defined(CPU64)}
  Result := true; // 64-битная программа запускается только на Win64
{$ELSEIF Defined(CPU16)}
  Result := false; // Win64 не поддерживает 16-разрядные приложения
{$ELSE}
  // 32-битные программы могут работать и на 32-разрядной и на 64-разрядной Windows
  // так что этот вопрос требует дальнейшего исследования
  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');

  Wow64Process := false;
  if Assigned(IsWow64Process) then
    Wow64Process := IsWow64Process(GetCurrentProcess, Wow64Process) and
      Wow64Process;

  Result := Wow64Process;
{$ENDIF}
end;

/// инициализация отображения строки прогресса поиска
procedure TMyTestTask.InitProgress;
begin
  FMyRegistry.CurProgress := 0;
  Write('Поиск:   0%');
  FMyRegistry.OnProgressChanged := ShowProgress;
end;

/// отображение текущего прогресса при поиске
procedure TMyTestTask.ShowProgress(Sender: TObject);
begin
  if FMyRegistry.CurProgress > 100 then
    Exit;
  Write(#8#8#8#8, FMyRegistry.CurProgress:3, '%');
end;

/// получение исходных данных от пользователя
procedure TMyTestTask.GetSourceData;
begin
  Writeln('Введите начальный узел для поиска (или оставьте значение пустым) и нажмите Enter:');
  Readln(FStartKeyName);
  Writeln('Введите подстроку для поиска в имени ключа (или оставьте значение пустым) и нажмите Enter:');
  Readln(FMyRegistry.TemplateStr);
end;

/// поиск и отображение результатов поиска в консоли
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
      Writeln(#13 + 'Ошибка поиска в реестре: ', E.Message);
  end;

  if FNotOpenedKeyNames.Count > 0 then
    for i := 0 to FNotOpenedKeyNames.Count - 1 do
      Writeln(Format(#13 + 'Ошибка чтения ключа %s', [FNotOpenedKeyNames[i]]));

  if FFoundKeyNames.Count > 0 then
  begin
    SaveSearchResult;
    Writeln(Format(#13 + 'Найдено ключей: %d. Результаты поиска в файле %s',
      [FFoundKeyNames.Count, RESULT_FILE_NAME]));
  end
  else
    Writeln(#13 + 'Ключи по шаблону не найдены.');
end;

/// запись результатов поиска в файл result.txt, расположенный рядом с .exe
procedure TMyTestTask.SaveSearchResult;
begin
  try
    FFoundKeyNames.SaveToFile(RESULT_FILE_NAME);
  except
    on E: Exception do
      Writeln('Ошибка записи результатов в файл: ', E.Message);
  end;
end;

///
var
  MyTestTask: TMyTestTask;

begin
  //Переключение консоли на кодовую страницу CP1251 (Windows-1251)
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

  Writeln('Нажмите Enter для выхода...');
  Readln;

end.
