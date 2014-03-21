unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DatabaseEntryDisplay1: TButton;
    DatabaseEntryDisplay2: TButton;
    DatabaseEntryDisplay3: TButton;
    DatabaseEntryDisplay4: TButton;
    DatabaseEntryDisplay5: TButton;
    DatabaseEntryDisplay6: TButton;
    AddButton: TButton;
    DatabaseEntryNameField: TEdit;
    ResetButton: TButton;
    SQLConnection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure SetupDatabase;
    procedure AddButtonClick(Sender: TObject);
    procedure DatabaseEntryDisplayClick(Sender: TObject);
    procedure SyncContent;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SyncContentButtons(SearchTag: integer; NewCpt: string);
    procedure RestoreButtonsToDefaultState;
  end;

var
  Form1 : TForm1;

  mEmptyDataSlotText : string = '';
  mDatabaseFull : string = 'Database is full!';
  mAlreadyExists : string = 'Already exists!'; // TODO: Implement this.

  // Database
  db_createTables : boolean;
  db_databaseName : string = 'test_database.db';
  db_tableName : string = 'test_table';
  db_columnPrimaryKeyAutoIncrement : string = 'test_id';
  db_columnName : string = 'test_values';

{$R *.lfm}

implementation

// Initialization
procedure TForm1.FormCreate(Sender: TObject);
begin
  SetupDatabase;
  SyncContent;
end;

procedure TForm1.SyncContent;
var
  pos : integer = 0;

begin
  // Removing single entries from database would be bugged if buttons won't
  // restored to their default state.
  RestoreButtonsToDefaultState;

  // sync content from database to the content buttons.
  SQLQuery.SQL.Text := 'SELECT * FROM "' + db_tableName + '"';
  SQLQuery.Open;
  SQLQuery.First; // move to the first record
  while(not SQLQuery.EOF) do
  begin
    // add the retrieved string to the list
    SyncContentButtons(pos, SQLQuery.FieldByName(db_columnName).AsString);

    // move to the next record
    SQLQuery.Next;
    pos := pos + 1;
  end;
  SQLQuery.Close;

  // Clear the value from the input field - prepare for next input
  DatabaseEntryNameField.Caption := mEmptyDataSlotText;
end;

// Helper method that changes the caption of the button, called by SyncContent
procedure TForm1.SyncContentButtons(SearchTag: integer; NewCpt: string);
var
  i: Integer;

begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TButton then
    begin
      if TButton(Components[i]).Tag = SearchTag then
         TButton(Components[i]).Caption := NewCpt;
         if TButton(Components[i]).Caption = NewCpt then
           TButton(Components[i]).Enabled := True;
    end;
end;

procedure TForm1.AddButtonClick(Sender: TObject);
begin
  // Check if there is room for new entry.
  if not (DatabaseEntryDisplay6.Caption = mEmptyDataSlotText) then
      DatabaseEntryNameField.Caption := mDatabaseFull
  else
  if not (Trim(DatabaseEntryNameField.Text) = '') then
      begin
        // Add entry to database with the value in the text field.
        SQLConnection.ExecuteDirect('INSERT INTO "' + db_tableName + '" VALUES '
                    + '(null, "' + Trim(DatabaseEntryNameField.Text) + '");');
        SQLTransaction.Commit;

        // Make sure the content displays up-to-date data.
        SyncContent;
      end
  else
      // Clear the value from the input field - prepare for next input even if
      // previous input was not succesfull.
      DatabaseEntryNameField.Caption := mEmptyDataSlotText;
end;

procedure TForm1.DatabaseEntryDisplayClick(Sender: TObject);
begin
  // TODO: remove entry from database with the value of the button's tag.
  SQLConnection.ExecuteDirect('DELETE FROM ' + db_tableName + ' WHERE '
                   + db_columnPrimaryKeyAutoIncrement + ' = "'
                   + IntToStr((Sender as TButton).Tag + 1) + '"');
  SQLTransaction.Commit;

  // Make sure the content displays up-to-date data.
  SyncContent;
end;

// This method performs a master deletion of everything.
procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  RestoreButtonsToDefaultState;

  // Remove everything from the database.
  SQLConnection.ExecuteDirect('DELETE FROM "' + db_tableName + '"');
  SQLTransaction.Commit;
end;

// Helper method that restores buttons to the default state, also called by
// SyncContent to fix removing single entries from database and then reflecting
// new values.
procedure TForm1.RestoreButtonsToDefaultState;
begin
  { Restore buttons to their initial state:
       Enabled: false,
       text: < Empty data slot >.}
  // TODO: improve this code by iteration (?)
  DatabaseEntryDisplay1.Caption := mEmptyDataSlotText;
  DatabaseEntryDisplay2.Caption := mEmptyDataSlotText;
  DatabaseEntryDisplay3.Caption := mEmptyDataSlotText;
  DatabaseEntryDisplay4.Caption := mEmptyDataSlotText;
  DatabaseEntryDisplay5.Caption := mEmptyDataSlotText;
  DatabaseEntryDisplay6.Caption := mEmptyDataSlotText;

  DatabaseEntryDisplay1.Enabled := False;
  DatabaseEntryDisplay2.Enabled := False;
  DatabaseEntryDisplay3.Enabled := False;
  DatabaseEntryDisplay4.Enabled := False;
  DatabaseEntryDisplay5.Enabled := False;
  DatabaseEntryDisplay6.Enabled := False;

  // Database is not full anymore, do not show any error.
  DatabaseEntryNameField.Caption := mEmptyDataSlotText;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLQuery.Close;
  SQLTransaction.Active := False;
  SQLConnection.Connected := False;
end;

procedure TForm1.SetupDatabase;
begin
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
      SQLiteLibraryName := './libsqlite3.so';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    SQLiteLibraryName := 'sqlite3.dll';
  {$ENDIF}

  SQLConnection.DatabaseName := GetAppConfigDir(false) + db_databaseName;

  // Check if config directory exists
  if not DirectoryExists(GetAppConfigDir(false)) then
    // If not: create it
    MkDir(GetAppConfigDir(false));

  // No file = setup database
  db_createTables := not FileExists(SQLConnection.DatabaseName);

  SQLConnection.Open;
  SQLTransaction.Active := true;

  if db_createTables then
    begin
      SQLConnection.ExecuteDirect('CREATE TABLE "' + db_tableName +'"('+
                    ' "' + db_columnPrimaryKeyAutoIncrement + '" INTEGER PRIMARY KEY,' +
                    ' "' + db_columnName + '" TEXT);');
      SQLTransaction.Commit;
    end;
end;

end.

