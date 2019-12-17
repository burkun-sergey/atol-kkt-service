unit SegmentArrayUnit;
//------------------------------------------------------------------------------
// Фасад для накопления данных сегментированных команд
//------------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  SegmentType=record
    guid: string;
    count: integer;
    segments: TStringList;
  end;

  SegmentArrayType=array of SegmentType;

  { TSegmentArray }

  TSegmentArray = class(TObject)
  private
    arr: SegmentArrayType;

    function getSegmentInd(const guid: string): integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure addSegment(const guid, text: string; const count: integer);
    function getGuidFullData(const guid: string; const del_after_get: boolean = true): string;
    procedure delSegment(const ind: integer);
  end;

var
  segments_arr: TSegmentArray;

implementation

{ TSegmentArray }

function TSegmentArray.getSegmentInd(const guid: string): integer;
var i: integer;
begin
  result := -1;

  for i:=0 to Length(arr)-1 do
    if AnsiCompareText(guid, arr[i].guid)=0 then
      begin
        result := i;
        break;
      end;
end;

constructor TSegmentArray.Create;
begin
  inherited;
end;

destructor TSegmentArray.Destroy;
var i: integer;
begin
  for i := 0 to Length(arr)-1 do
    arr[i].segments.Free;
  SetLength(arr, 0);

  inherited Destroy;
end;

procedure TSegmentArray.addSegment(const guid, text: string; const count: integer);
var ind: integer;
begin
  ind := getSegmentInd(guid);
  if ind<0 then
    begin
      SetLength(arr, Length(arr)+1);
      ind := Length(arr)-1;
      arr[ind].segments := TStringList.create;
    end;

  arr[ind].segments.Add(text);
  arr[ind].guid := guid;
  arr[ind].count := count;
end;

function TSegmentArray.getGuidFullData(const guid: string;
  const del_after_get: boolean): string;
var ind: integer;
begin
  result := '';
  ind := getSegmentInd(guid);
  if ind >= 0 then
    if arr[ind].count=arr[ind].segments.Count then
      begin
        result := StringReplace(arr[ind].segments.Text, #13#10, '', [rfReplaceAll]);
        if del_after_get then
          delSegment(ind);
      end;
end;

procedure TSegmentArray.delSegment(const ind: integer);
var i: integer;
begin
  if not ((ind >= 0) and (ind < Length(arr))) then
    exit;

  arr[ind].segments.Free;

  if Length(arr) > 1 then
    for i := ind to Length(arr)-1 do
      arr[i] := arr[i+1];

  SetLength(arr, Length(arr)-1);
end;

end.

