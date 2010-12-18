require 'file_entry'

describe "FileEntry for file in directory" do
  before(:each) do
    @entry = FileEntry.new("/path/child/name")
  end
  
  it "is a file" do
    @entry.should be_file
  end

  it "is not a directory" do
    @entry.should_not be_directory
  end

  it "has path with which it was initialized" do
    @entry.path.should == "/path/child/name"
  end

  it "considers name to be text right of last forwar slash" do
    @entry.name.should == "name"
  end
end


describe "FileEntry for file without specified parent directory" do
  before(:each) do
    @entry = FileEntry.new("file_name")
  end
  
  it "has name equal to specified path" do
    @entry.name.should == "file_name"
  end
end
