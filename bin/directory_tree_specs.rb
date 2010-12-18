require 'directory_tree'

describe DirectoryTree do
  before(:each) do
    @tree = DirectoryTree.new
  end

  it "has no entries for empty directory" do
    set_entries_for_path("/path", [], [])
    @tree.load_path("/path")
    @tree.entries.size.should == 0
  end

  it "has single file entry for directory containing single file" do
    set_entries_for_path("/path", [], ["file_name"])

    @tree.load_path("/path")

    @tree.entries.size.should == 1
    tree_should_have_file_entry(1, "/path/file_name", "file_name")
  end

  it "has file entry for each file in directory containing multiple files" do
    set_entries_for_path("/path", [], ["file_name", "other_file_name"])

    @tree.load_path("/path")

    @tree.entries.size.should == 2
    tree_should_have_file_entry(1, "/path/file_name", "file_name")
    tree_should_have_file_entry(1, "/path/file_name", "file_name")
  end

  it "has single directory entry for directory containing single directory" do
    set_entries_for_path("/path", ["sub_directory"], [])

    @tree.load_path "/path"

    @tree.entries.size.should == 1
    @tree.entries.first.directory?.should be(true)
    @tree.entries.first.name.should == "sub_directory"
    @tree.entries.first.path.should == "/path/sub_directory/"
  end

  # it "adds entries for files in sub directories" do
  #   set_entries_for_path("/path", ["child"], ["file_name"])
  #   set_entries_for_path("/path/child", ["grand_child"], ["child_file"])
  #   set_entries_for_path("/path/child/grand_child", [], ["grand_child_file"])
  #   @tree.load_path("/path")

  #   @tree.entries.size.should == 3
  #   should_have_file_entry(1, "/path/file_name", "file_name")
  #   should_have_file_entry(1, "/path/file_name", "file_name")
  # end

  def tree_should_have_file_entry(index, path, name)
    @tree.entries.first.file?.should be(true)
    @tree.entries.first.name.should == name
    @tree.entries.first.path.should == path
  end

  # use spat hash in 1.9 to allow for calls looking like this:
  # set_entries_for_path(path: "/path", files: [], directories: [])
  def set_entries_for_path(path, directories, files)
    files_and_directories = directories.concat files
    fake_entries = [".", ".."].concat files_and_directories

    stub = Dir.should_receive(:foreach).at_least(:once)
    fake_entries.each { |e| stub.and_yield(e) }

    directories.each { |entry| File.stub!(:file?).with(entry).and_return(false) }
    files.each { |entry| File.stub!(:file?).with(entry).and_return(true) }
  end
end

