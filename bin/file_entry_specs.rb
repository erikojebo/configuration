require 'file_entry'

describe "FileEntry" do
  before(:each) do
    @entry = FileEntry.new("name")
  end
  
  it "is a file" do
    @entry.should be_file
  end

  it "is not a directory" do
    @entry.should_not be_directory
  end

end
