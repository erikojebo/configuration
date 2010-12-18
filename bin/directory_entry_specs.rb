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

describe "Directory entry for sub directory without trailing shash" do
  before(:each) do
    @entry = DirectoryEntry.new("/path/sub_directory")
  end

  it "adds trailing slash to path" do
    @entry.path.should == "/path/sub_directory/"
  end

  it "considers path after last forward slash to be the name" do
    @entry.name.should == "sub_directory"
  end
end

describe "Directory entry for sub directory with trailing shash" do
  before(:each) do
    @entry = DirectoryEntry.new("/path/sub_directory/")
  end

  it "has single trailing slash in path" do
    @entry.path.should == "/path/sub_directory/"
  end

  it "excludes trailing slash from name" do
    @entry.name.should == "sub_directory"
  end
end

describe "Directory entry for relative path without parent" do
  before(:each) do
    @entry = DirectoryEntry.new("sub_directory/")
  end

  it "considers name to be path except trailing slash" do
    @entry.name.should == "sub_directory"
  end
end
