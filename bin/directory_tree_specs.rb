require 'directory_tree'

class ContainsFileMatcher
  def initialize(path, name)
    @expected_path = path
    @expected_name = name
  end

  def matches?(directory_entry)
    directory_entry.files.each do |file| 
      if file.path == @expected_path && 
          file.name == @expected_name &&
          file.file?
        return true
      end
    end

    @all_files = directory_entry.files.map { |f| "Path: #{f.path}, Name: #{f.name}" } * "\n"

    false
  end
  
  def description
    "contains file"
  end
  
  def failure_message
    " expected to contain file, but no file with matching path and name was found\n" + 
      "Actual files:\n#{@all_files}"
  end
  
  def negative_failure_message
    " expected not to contain file, but a file with matching name and path was found\n" + 
      "Actual files:\n#{@all_files}"
  end
end

def contain_file(path, name)
  ContainsFileMatcher.new(path, name)
end

class ContainsDirectoryMatcher
  def initialize(path, name)
    @expected_path = path
    @expected_name = name
  end

  def matches?(directory_entry)
    directory_entry.directories.each do |directory| 
      if directory.path == @expected_path && 
          directory.name == @expected_name &&
          directory.directory?
        return true
      end
    end

    @all_directories = directory_entry.directories.map { |d| "Path: #{d.path}, Name: #{d.name}" } * "\n"

    false
  end
  
  def description
    "contains directory"
  end
  
  def failure_message
    " expected to contain directory, but no directory with matching path and name was found\n" + 
      "Actual directories:\n#{@all_directories}"
  end
  
  def negative_failure_message
    " expected not to contain directory, but a directory with matching name and path was found\n" + 
      "Actual directories:\n#{@all_directories}"
  end
end

def contain_directory(path, name)
  ContainsDirectoryMatcher.new(path, name)
end

# use splat hash in 1.9 to allow for calls looking like this:
# set_entries_for_path(path: "/path", files: [], directories: [])
def set_entries_for_path(path, directories, files)
  files_and_directories = directories.concat files
  fake_entries = [".", ".."].concat files_and_directories

  stub = Dir.should_receive(:foreach).at_least(:once)
  fake_entries.each { |e| stub.and_yield(e) }

  directories.each { |entry| File.stub!(:file?).with(entry).and_return(false) }
  files.each { |entry| File.stub!(:file?).with(entry).and_return(true) }
end

describe "FileHierarchyReader reading directory containing single one file and one directory" do
  before(:each) do
    @tree = FileHierarchyReader.new
    set_entries_for_path("/path", ["sub_directory"], ["file_name"])
    @dir = @tree.read "/path"
  end

  it "has single file entry" do
    @dir.files.size.should == 1
    @dir.should contain_file("/path/file_name", "file_name")
  end

  it "has single directory" do
    @dir.directories.size.should == 1
    @dir.should contain_directory("/path/sub_directory/", "sub_directory")
  end
end

describe "FileHierarchyReader reading empty directory" do
  before(:each) do
    @tree = FileHierarchyReader.new
    set_entries_for_path("/path", [], [])
    @dir = @tree.read("/path")
  end

  it "has no entries for empty directory" do
    @dir.entries.size.should == 0
  end
end

describe "FileHierarchyReader reading directory with multiple files and directories" do
  before(:each) do
    @tree = FileHierarchyReader.new
    set_entries_for_path("/path", ["sub_directory", "other_sub_directory"], ["file_name", "other_file_name"])
    @dir = @tree.read("/path")
  end

  it "has file entry for each file" do
    @dir.files.size.should == 2
    @dir.should contain_file("/path/file_name", "file_name")
    @dir.should contain_file("/path/other_file_name", "other_file_name")
  end

  it "has directory entry for each directory" do
    @dir.directories.size.should == 2
    @dir.should contain_directory("/path/sub_directory/", "sub_directory")
    @dir.should contain_directory("/path/other_sub_directory/", "other_sub_directory")
  end
end

describe "FileHierarchyReader reading directory with sub directories containing files" do
  before(:each) do
    set_entries_for_path("/path", ["child"], ["file_name"])
    set_entries_for_path("/path/child", ["grand_child"], ["child_file"])
    set_entries_for_path("/path/child/grand_child", [], ["grand_child_file"])

    @tree = FileHierarchyReader.new
    @dir = @tree.read("/path")
  end

  it "adds entries for files in sub directories" do
    @tree.entries.size.should == 3
    @dir.should contain_file("/path/file_name", "file_name")
    @dir.should contain_file("/path/child/child_file", "child_file")
    @dir.should contain_file("/path/child/grand_child/grand_child_file", "grand_child_file")
  end
end
