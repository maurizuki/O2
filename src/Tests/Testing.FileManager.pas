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
  end;

implementation

uses
  uServices, uFileManager;

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

end.
