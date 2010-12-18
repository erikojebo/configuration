require 'directory_entry'

describe "Directory entry" do
  before(:each) do
    @files = [ FileEntry.new("file1"), FileEntry.new("file2") ]
    @directories = [ DirectoryEntry.new("/dir1"), DirectoryEntry.new("/dir2") ]
    @entry = DirectoryEntry.new("/path", @directories, @files)
  end

  it "is a directory" do
    @entry.should be_directory
  end

  it "is not a file" do
    @entry.should_not be_file
  end

  it "contains files with which it was initialized" do
    @entry.files.first.should == @files.first
    @entry.files.last.should == @files.last
  end

  it "contains directories with which it was initialized" do
    @entry.directories.first.should == @directories.first
    @entry.directories.last.should == @directories.last
  end

  it "has entries collection which consists of all its directories followed by all its files" do
    @entry.entries.size.should == 4
    @entry.entries[0].should == @directories[0]
    @entry.entries[1].should == @directories[1]
    @entry.entries[2].should == @files[0]
    @entry.entries[3].should == @files[1]
  end
end


describe "Directory entry created without files or sub directories" do
  before(:each) do
    @entry = DirectoryEntry.new("path")
  end
  
  it "contains no files" do
    @entry.files.size.should == 0
  end

  it "contains no directories" do
    @entry.directories.size.should == 0
  end

  it "has total directory count 0" do
    @entry.total_directory_count.should == 0
  end

  it "has total file count 0" do
    @entry.total_file_count.should == 0
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

describe "Directory entry containing 1 file and 1 sub directory" do
  before(:each) do
    sub_dir = DirectoryEntry.new("/path/sub_dir")
    file = FileEntry.new("/path/file")
    @entry = DirectoryEntry.new("/path", [ sub_dir ], [ file ])
  end

  it "has total file count 1" do
    @entry.total_file_count.should == 1
  end

  it "has total directory count 1" do
    @entry.total_directory_count.should == 1
  end
end

describe "Directory entry containing 2 files and 2 nested sub directories with 2 files each" do
  before(:each) do
    sub_dir = DirectoryEntry.new("/path/sub_dir")
    file1 = FileEntry.new("/path/file1")
    file2 = FileEntry.new("/path/file2")
    child_file1 = FileEntry.new("/path/childfile1")
    child_file2 = FileEntry.new("/path/childfile2")
    grand_file1 = FileEntry.new("/path/childfile1")
    grand_file2 = FileEntry.new("/path/childfile2")

    grand_child_entry = DirectoryEntry.new("/path/child/grand_child", [], [grand_file1, grand_file2])
    child_entry = DirectoryEntry.new("/path/child", [grand_child_entry], [child_file1, child_file2])
    @entry = DirectoryEntry.new("/path", [ child_entry ], [ file1, file2 ])
  end

  it "has total file count 6" do
    @entry.total_file_count.should == 6
  end

  it "has total directory count 2" do
    @entry.total_directory_count.should == 2
  end
end
