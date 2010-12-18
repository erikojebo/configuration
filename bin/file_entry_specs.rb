require 'file_entry'

describe "FileEntry" do
  before(:each) do
    @entry = FileEntry.new("name")
  end
  
  it "is a file" do
    @entry.file?.should be(true)
  end

  it "is not a directory" do
    @entry.directory?.should be(false)
  end

end
