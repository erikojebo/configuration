require 'directory_tree'
require 'file_hierarchy_reader_spec_helpers'

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

describe "FileHierarchyReader reading directory with two nested sub directories containing one file each" do
  before(:each) do
    set_entries_for_path("/path", ["child"], ["file_name"])
    set_entries_for_path("/path/child", ["grand_child"], ["child_file"])
    set_entries_for_path("/path/child/grand_child", [], ["grand_child_file"])

    @tree = FileHierarchyReader.new
    @dir = @tree.read("/path")
  end

  it "adds entries for file in first level sub directory" do
    @dir.directories.size.should == 1

    child_dir = @dir.directories.first
    child_dir.files.size.should == 1
    child_dir.should contain_file("/path/child/child_file", "child_file")
  end
end
