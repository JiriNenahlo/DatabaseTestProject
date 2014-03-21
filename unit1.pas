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

    // Do not set onEditingDone event for this one, messes up everything. Thrust me.
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
    procedure RestoreContentToDefaultState;
  end;

var
  Form1 : TForm1;

  s_empty : string = '';
  s_databaseFull : string = 'Database is full!';
  s_alreadyExists : string = 'Already exists!';
  s_inputIsEmpty : string = 'Input is empty!';

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
  // be restored to their default state.
  RestoreContentToDefaultState;

  // Sync content from database to the content buttons.
  SQLQuery.SQL.Text := 'SELECT * FROM "' + db_tableName + '"';
  SQLQuery.Open;
  SQLQuery.First; // Move to the first record.
  while(not SQLQuery.EOF) do
  begin
    // Add the retrieved string to the list.
    SyncContentButtons(pos, SQLQuery.FieldByName(db_columnName).AsString);

    // Move to the next record.
    SQLQuery.Next;
    pos := pos + 1;
  end;
  SQLQuery.Close;

  // Clear the value from the input field - prepare for next input.
  DatabaseEntryNameField.Caption := s_empty;
end;

// Helper method that changes the caption of the button, called by SyncContent.
procedure TForm1.SyncContentButtons(SearchTag: integer; NewCpt: string);
var
  currComponentCount: Integer;

begin
  for currComponentCount := 0 to ComponentCount - 1 do
    if Components[currComponentCount] is TButton then
    begin
      if TButton(Components[currComponentCount]).Tag = SearchTag then
         TButton(Components[currComponentCount]).Caption := NewCpt;
         if TButton(Components[currComponentCount]).Caption = NewCpt then
           TButton(Components[currComponentCount]).Enabled := True;
    end;
end;

procedure TForm1.AddButtonClick(Sender: TObject);
var
  currComponentCount,
  currSuffix: Integer;
  isDuplicate : boolean = False;

begin
  // Check if the entry is not duplicate. This is not very useful as originally
  // thought, because entries get deleted not based on the entry text, but the
  // rowid, which is an INTEGER PRIMARY KEY (gets automatically incremented
  // with each new entry and is unique), so there's no chance that the duplicate
  // name will affect the deleting operation later on)
  // However, leaving it here won't hurt and it seems to work as intended.
  for currComponentCount := 0 to ComponentCount - 1 do
      if (Components[currComponentCount] is TButton) then
         // DatabaseEntryDisplay1-6.
         for currSuffix := 1 to 6 do
           begin
             if Components[currComponentCount].Name = 'DatabaseEntryDisplay' + IntToStr(currSuffix) then
                begin
                  if (TButton(Components[currComponentCount]).Caption = DatabaseEntryNameField.Text) then
                    isDuplicate := True;
                end;
           end;

  // Check if there is room for new entry.
  if not (DatabaseEntryDisplay6.Caption = s_empty) then
      DatabaseEntryNameField.Text := s_databaseFull
  else
  // Check if the entry is duplicate (see above)
  if (Trim(DatabaseEntryNameField.Text) = '') then
     DatabaseEntryNameField.Text := s_inputIsEmpty
  else
  // Check if the input is not just empty (or spaces).
  if (isDuplicate) then
    DatabaseEntryNameField.Text := s_alreadyExists
  else
      begin
        // Add entry to database with the value in the text field.
        SQLConnection.ExecuteDirect('INSERT INTO "' + db_tableName + '" VALUES '
                    + '(null, "' + Trim(DatabaseEntryNameField.Text) + '");');
        SQLTransaction.Commit;

        // Make sure the content displays up-to-date data.
        SyncContent;
      end;
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
  RestoreContentToDefaultState;

  // Remove everything from the database.
  SQLConnection.ExecuteDirect('DELETE FROM "' + db_tableName + '"');
  SQLTransaction.Commit;
end;

// Helper method that restores buttons to the default state, also called by
// SyncContent to fix removing single entries from database and then reflecting
// new values.
procedure TForm1.RestoreContentToDefaultState;
var
  currComponentCount,
  currSuffix: Integer;

begin
  { Restore buttons to their initial state:
       enabled: false,
       text: < Empty data slot >. }
  for currComponentCount := 0 to ComponentCount - 1 do
    if (Components[currComponentCount] is TButton) then
       // DatabaseEntryDisplay1-6.
       for currSuffix := 1 to 6 do
         begin
           if Components[currComponentCount].Name = 'DatabaseEntryDisplay' + IntToStr(currSuffix) then
              begin
              TButton(Components[currComponentCount]).Caption := s_empty;
              TButton(Components[currComponentCount]).Enabled := False;
              end;
         end;

  // Prepare the field for next input.
  DatabaseEntryNameField.Text := s_empty;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLQuery.Close;
  SQLTransaction.Active := False;
  SQLConnection.Connected := False;
end;

procedure TForm1.SetupDatabase;
begin
  // Use supplied SQLite3 database file for windows with this program to avoid
  // crash on systems without SQLite3 installed.
  {$IFDEF WINDOWS}
    SQLiteLibraryName := 'sqlite3.dll';
  {$ENDIF}

  // With Linux it should work fine out-of-the-box (needs testing).
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
      SQLiteLibraryName := './libsqlite3.so';
    {$ENDIF}
  {$ENDIF}

  SQLConnection.DatabaseName := GetAppConfigDir(false) + db_databaseName;

  // Check if config directory exists
  if not DirectoryExists(GetAppConfigDir(false)) then
    // If not, create it
    MkDir(GetAppConfigDir(false));

  // No file = setup database
  db_createTables := not FileExists(SQLConnection.DatabaseName);

  SQLConnection.Open;
  SQLTransaction.Active := true;

  if db_createTables then
    begin
      SQLConnection.ExecuteDirect('CREATE TABLE "' + db_tableName + '"(' +
                    ' "' + db_columnPrimaryKeyAutoIncrement + '" INTEGER PRIMARY KEY,' +
                    ' "' + db_columnName + '" TEXT);');
      SQLTransaction.Commit;
    end;
end;

end.

