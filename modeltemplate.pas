unit modeltemplate;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, sqldb, maindm;
const CRLF = #13#10;

type
  TGetSet = (tGET, tSET);

function ModelGetterSetter(AFieldType, AFieldName: String): String;
function ModelFields(AModelId: Integer): String;
function TimeStampAnnotation(AFieldType: String): String;
function GetTemplate(AFileName: String): String;
function ColumnAnnotation(AColumnName: String): String;
function ReplaceStr(AStr, AWhat, AWith: String): String;
function ModelAnnotation(ATableName: String): String;

function GenerateModelSource(AModelId: Integer): String;

implementation

function GenerateModelSource(AModelId: Integer): String;
var
  ModelQuery: TSQLQuery;
  Template, ModelName: String;
begin
  Template := GetTemplate('./template/model.template');
  Result := '';
  ModelQuery := MainDataModule.CreateQuery('select * from model where id = :id ');
  try
    ModelQuery.Params.ParamByName('id').AsInteger := AModelId;
    ModelQuery.Open;
    if not ModelQuery.EOF then
    begin
      ModelName := ModelQuery.FieldByName('name').AsString;
      Template := ReplaceStr(Template, '#MODELNAME#', ModelName);
      Template := ReplaceStr(Template, '#MODEL_ANNOTATION#', ModelAnnotation(ModelQuery.FieldByName('tablename').AsString));
      Template := ReplaceStr(Template, '#MODEL_FIELDS#', ModelFields(ModelQuery.FieldByName('id').AsInteger));
    end;
  finally
    ModelQuery.Close;
    ModelQuery.Free;
  end;
  Result := Template;
end;

function ModelAnnotation(ATableName: String): String;
begin
  Result := Format('@Table(name="%s")', [ATableName]);
end;

function ModelFields(AModelId: Integer): String;
var
  AttributeQuery: TSQLQuery;
  FieldName, FieldType: String;
  GetterSetterSource, FieldDeclarationSource: String;
begin
  AttributeQuery := MainDataModule.CreateQuery('select * from attribute where modelid = :id ');
  try
    GetterSetterSource := CRLF;
    FieldDeclarationSource := CRLF;
    AttributeQuery.Params.ParamByName('id').AsInteger := AModelId;
    AttributeQuery.Open;
    while not AttributeQuery.EOF do
    begin
      FieldName := AttributeQuery.FieldByName('name').AsString;
      FieldType := AttributeQuery.FieldByName('fieldtype').AsString;
      FieldDeclarationSource := FieldDeclarationSource + TimeStampAnnotation(FieldType);
      FieldDeclarationSource := FieldDeclarationSource + ColumnAnnotation(AttributeQuery.FieldByName('fieldname').AsString);
      FieldDeclarationSource := FieldDeclarationSource + '  private ' + FieldType + ' ' + FieldName + ';' + CRLF + CRLF ;
      GetterSetterSource := GetterSetterSource + ModelGetterSetter(FieldType, FieldName) + CRLF;
      AttributeQuery.Next;
    end;
  finally
    AttributeQuery.Close;
    AttributeQuery.Free;
  end;
  Result := FieldDeclarationSource + GetterSetterSource;
end;

function TimeStampAnnotation(AFieldType: String): String;
begin
  Result := '';
  if LowerCase(AFieldType) = 'timestamp' then
    Result := '  @Temporal(TemporalType.TIME)' + CRLF +
      '  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd.MM.yyyy HH:mm")' + CRLF
  else if LowerCase(AFieldType) = 'time' then
    Result := '  @Temporal(TemporalType.TIME)' + CRLF+
      '  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "HH:mm")'+ CRLF
  else if LowerCase(AFieldType) = 'date' then
    Result :='  @Temporal(TemporalType.TIME)' + CRLF +
      '  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd.MM.yyyy")'+ CRLF;
end;

function ColumnAnnotation(AColumnName: String): String;
begin
  if AColumnName <> '' then
    Result := Format('  @Column(name="%s")', [AColumnName]) + CRLF
  else Result := '';
end;

function MappedFieldType(AFieldType: String): String;
begin
  Result := AFieldType;
  if LowerCase(AFieldType) = 'timestamp' then
    Result := 'Date'
  else if LowerCase(AFieldType) = 'time' then
    Result := 'Date'
  else if LowerCase(AFieldType) = 'date' then
     Result := 'Date'
end;

function FunctionName(AFieldName, AFieldType: String; AFuncType: TGetSet): String;
var
  Prefix: String;
begin
  if (AFuncType = tGET) then
  begin
    if LowerCase(AFieldType) = LowerCase('boolean') then
      Prefix := 'is'
    else
      Prefix := 'get';
  end else
    Prefix := 'set';
  Result := Prefix + UpperCase(copy(AFieldName, 1, 1)) + copy(AFieldName, 2, Length(AFieldName))
end;

function ReplaceTags(AText, AFieldType, AFieldName: String): String;
var
  JavaFieldType: String;
begin
  JavaFieldType := MappedFieldType(AFieldType);
  Result := AText;
  Result := ReplaceStr(Result , '#GETFUNCTIONNAME#', FunctionName(AFieldName, AFieldType, tGET));
  Result := ReplaceStr(Result , '#FIELDTYPE#', JavaFieldType);
  Result := ReplaceStr(Result , '#FIELDNAME#', AFieldName);

  Result := ReplaceStr(Result , '#SETFUNCTIONNAME#', FunctionName(AFieldName, AFieldType, tSET));
end;

function GetTemplate(AFileName: String): String;
var
  TemplateFile: TextFile;
  line: string;
begin
  AssignFile(TemplateFile, AFileName);
  Result := '';
  try
    Reset(TemplateFile);
    while not EOF(TemplateFile) do
    begin
      Readln(TemplateFile, line);
      Result := Result + line + CRLF;
    end;
  finally
    CloseFile(TemplateFile)
  end;
end;

function ModelGetterSetter(AFieldType, AFieldName: String): String;
var
  Template: string;
begin
  Template := GetTemplate('./template/getsetfunctions.template');
  Result := ReplaceTags(Template, AFieldType, AFieldName);
end;

function ReplaceStr(AStr, AWhat, AWith: String): String;
begin
  Result := stringReplace(AStr , AWhat, AWith, [rfReplaceAll, rfIgnoreCase]);
end;



end.

