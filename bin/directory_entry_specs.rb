require 'directory_entry'


describe "Directory entry" do
  before(:each) do
    @entry = DirectoryEntry.new("/path")
  end

  it "is a directory" do
    @entry.directory?.should be(true)
  end

  it "is not a file" do
    @entry.file?.should be(false)
  end
end

describe "Directory entry with sub directory without trailing shash" do
  before(:each) do
    @entry = DirectoryEntry.new("/path")
  end

  it "adds trailing slash to path" do
    @entry.path.should == "/path/sub_directory/"
  end

  it "considers path after last forward slash to be the name" do
    @entry.name.should == "sub_directory"
  end
end
