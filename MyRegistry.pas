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
    class var CurProgress: integer; // текущее значение прогресса поиска
    class var OnProgressChanged: TNotifyEvent; // событие изменения прогресса
    class var TemplateStr: string; // шаблонная строка для поиска

    /// регистронезависимый поиск по шаблону в заданном StartKeyName и всех
    /// вложенных (по отношению к заданному) ключах реестра
    procedure SearchKeysByTemplate(const StartKeyName: string;
      // имя узла, в котором осуществляется поиск
      FoundKeyNames, // перечень ключей, соответствующих критерию поиска
      NotOpenedKeys: TStringList; // перечень ключей, которые не были открыты
      StepProgress: integer);
    // часть от общего прогресса поиска для текущего узла
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

    // открытие заданного узла
    if MyReg.OpenKeyReadOnly(StartKeyName) then
      try
        Keys := TStringList.Create;
        try
          MyReg.GetKeyNames(Keys); // получение списка вложенных ключей
          if Keys.Count > 0 then
          begin
            StepProgress := Trunc(StepProgress / Keys.Count);
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
