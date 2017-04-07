unit maindm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, db, FileUtil;

type

  { TMainDM }

  TMainDM = class(TDataModule)
    AttributeDS: TDataSource;
    AttributeQuery: TSQLQuery;
    Connection: TSQLite3Connection;
    ModelDS: TDataSource;
    ModelQuery: TSQLQuery;
    ProjectDS: TDataSource;
    ProjectQuery: TSQLQuery;
    Transaction: TSQLTransaction;

    procedure AttributeQueryAfterInsert(DataSet: TDataSet);
    procedure ModelQueryAfterInsert(DataSet: TDataSet);
    procedure ModelQueryAfterScroll(DataSet: TDataSet);
  private
    procedure OpenAttributeQuery;
    procedure OpenModelQuery;
    { private declarations }
  public
    procedure SaveAllChanges;
    procedure ActivateConnectionAndAllQueries;
    procedure CloseConnection;
    function CreateQuery(ASql: String): TSQLQuery;
  end;
var
  MainDataModule: TMainDM;

implementation

{$R *.lfm}

{ TMainDM }
procedure TMainDM.ActivateConnectionAndAllQueries;
begin
  Connection.Open;
  Transaction.Active := True;
  ProjectQuery.Active := True;
  OpenModelQuery;
  OpenAttributeQuery;
end;

procedure TMainDM.CloseConnection;
begin
  Connection.Close(true);
end;

procedure TMainDM.SaveAllChanges;
begin
   try
    if Transaction.Active then
    begin
      ProjectQuery.ApplyUpdates;
      ModelQuery.ApplyUpdates;
      AttributeQuery.ApplyUpdates;
      Transaction.Commit;
      ActivateConnectionAndAllQueries;
    end;
  except
  on E: EDatabaseError do
    begin
      raise Exception.Create('A database error has occurred. Technical error message: ' + E.Message);
    end;
  end;
end;

procedure TMainDM.OpenModelQuery;
begin
  ModelQuery.Active := False;
  ModelQuery.Params.ParamByName('id').AsInteger := ProjectQuery.FieldByName('id').AsInteger;
  ModelQuery.Active := True;
end;

procedure TMainDM.OpenAttributeQuery;
begin
  AttributeQuery.Active := False;
  AttributeQuery.Params.ParamByName('id').AsInteger := ModelQuery.FieldByName('id').AsInteger;
  AttributeQuery.Active := True;
end;

procedure TMainDM.ModelQueryAfterScroll(DataSet: TDataSet);
begin
  OpenAttributeQuery;
end;

procedure TMainDM.ModelQueryAfterInsert(DataSet: TDataSet);
begin
  ModelQuery.FieldByName('projectid').AsInteger := ProjectQuery.FieldByName('id').AsInteger;
end;

procedure TMainDM.AttributeQueryAfterInsert(DataSet: TDataSet);
begin
  AttributeQuery.FieldByName('modelid').AsInteger := ModelQuery.FieldByName('id').AsInteger;
  AttributeQuery.FieldByName('fieldtype').AsString := 'String';
end;

function TMainDM.CreateQuery(ASql: String): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.SQL.Add(ASql);
  Result.Database := Connection;
  Result.Transaction := Transaction;
end;

end.

