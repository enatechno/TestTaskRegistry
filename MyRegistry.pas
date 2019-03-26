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
    class var CurProgress: Extended; // текущее значение прогресса поиска
    class var ProgressValue: integer;  // округленное значение прогресса
    class var OnProgressChanged: TNotifyEvent; // событие изменения прогресса
    class var TemplateStr: string; // шаблонная строка для поиска

    procedure InitProgress;

    /// <summary>
    /// Регистронезависимый поиск по шаблону в заданном StartKeyName и всех
    /// вложенных (по отношению к заданному) ключах реестра
    /// </summary>
    /// <param name='StartKeyName'>имя узла, в котором осуществляется поиск</param>
    /// <param name='FoundKeyNames'>список найденных ключей, соответствующих критерию поиска</param>
    /// <param name='NotOpenedKeys'>список ключей, которые не были открыты</param>
    /// <param name='StepProgress'>часть (в %) от общего прогресса поиска для текущего узла</param>
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
    // открытие заданного узла
    if MyReg.OpenKeyReadOnly(StartKeyName) then
      try
        Keys := TStringList.Create;
        try
          MyReg.GetKeyNames(Keys); // получение списка вложенных ключей
          if Keys.Count > 0 then
          begin
            StepProgress := StepProgress / Keys.Count;
            for I := 0 to Keys.Count - 1 do
            begin
              // получение полного имени ключа
              KeyName := Format('%s\%s', [StartKeyName, Keys[I]]);

              // проверка наличия шаблонной подстроки в имени ключа из списка
              if (TemplateStr = '') or // (без учета регистра)
                (Pos(UpperCase(TemplateStr), UpperCase(Keys[I])) > 0) then
              begin
                FoundKeyNames.Add(KeyName);
              end;

              // рекурсивный вызов - поиск во вложенных ключах
              MyReg.SearchKeysByTemplate(KeyName, FoundKeyNames, NotOpenedKeys,
                StepProgress);
            end;
          end
          else
            // если узел не имеет вложенных ключей, то работа с ним завершена и
            // часть отведенного ему прогресса добавляется в общий
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

      // не открывшийся узел - это тоже прогресс
      // (за исключением когда не открылся самый первый узел у которого StepProgress = 100)
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

  if ProgressValue = i then  // изменилось ли округленное значение прогресса
    Exit;

  ProgressValue := i;

  if Assigned(OnProgressChanged) then
    OnProgressChanged(nil);
end;

end.
