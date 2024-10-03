unit Testing.FileManager;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFileManagerTests = class
  public
    [Test]
    procedure LoadTags;

    [Test]
    procedure FilterByName;
  end;

implementation

uses
  uServices, uFileManager, uO2Objects;

{ TFileManagerTests }

procedure TFileManagerTests.LoadTags;
var
  Model: IFileManager;
begin
  Model := TFileManager.Create(nil, nil);

  Model.O2File.Objects.AddObject('Object 1').Tag := 'Tag 1,Tag 2';
  Model.O2File.Objects.AddObject('Object 2').Tag := 'tag 2,Tag 3';

  Assert.AreEqual(4, Model.Tags.Count);
  Assert.Contains(Model.Tags, 'Tag 1');
  Assert.Contains(Model.Tags, 'Tag 2');
  Assert.Contains(Model.Tags, 'Tag 3');
end;

procedure TFileManagerTests.FilterByName;
var
  Model: IFileManager;
  AObject: TO2Object;
begin
  Model := TFileManager.Create(nil, nil);

  Model.O2File.Objects.AddObject('A matching object');
  Model.O2File.Objects.AddObject('Another object');

  Model.ObjectName := 'Matching';

  for AObject in Model.GetObjects do
    Assert.AreEqual('A matching object', AObject.Name);
end;

end.
