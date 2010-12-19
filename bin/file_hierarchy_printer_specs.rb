require 'file_hierarchy_printer'
require 'directory_entry'

class StringContainsMatcher
  def initialize(expected_substring)
    @expected_substring = expected_substring
  end

  def matches?(actual)
    @actual = actual
    actual =~ /#{Regexp.escape(@expected_substring)}/m
  end

  def description
    "contains substring"
  end

  def failure_message
    "expected string to contain '#{@expected_substring}' but was '#{@actual}'"
  end

  def negative_failure_message
    "expected string to NOT contain '#{@expected_substring}' but was '#{@actual}'"
  end
end

def contain(string)
  StringContainsMatcher.new(string)
end

describe "FileHierarchyPrinter printing empty folder" do
  before(:each) do
    printer = FileHierarchyPrinter.new
    dir = DirectoryEntry.new('path')
    @output = printer.print_to_string(dir)
  end
  
  it "prints . as tree" do
    @output.should contain(".
")
  end

  it "prints summary with zero directories and zero files" do
    @output.should contain("0 directories, 0 files")
  end
end

describe "FileHierarchyPrinter printing directory with single file" do
  before(:each) do
    file = FileEntry.new("/path/File name")
    dir = DirectoryEntry.new('/path', [], [ file ])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints single file name under root dot" do
    @output.should contain(".
`-- File name
")
  end

 it "prints '0 directories, 1 file' as summary" do
    @output.should contain("0 directories, 1 file")
  end
end

describe "FileHierarchyPrinter printing directory with single sub directory" do
  before(:each) do
    sub_dir = DirectoryEntry.new("/path/sub_directory/")
    dir = DirectoryEntry.new('/path', [ sub_dir ], [])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints single directory name under root dot" do
    @output.should contain(".
`-- sub_directory
")
  end

  it "prints '1 directory, 0 files' as summary" do
    @output.should contain("1 directory")
  end
end

describe "FileHierarchyPrinter printing directory with 1 file and 1 directory" do
  before(:each) do
    sub_dir = DirectoryEntry.new("/path/sub_directory/")
    file = FileEntry.new("/path/file_name")
    dir = DirectoryEntry.new('/path', [ sub_dir ], [ file ])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints file name before directory name, directly under root dot" do
    @output.should contain(
".
|-- file_name
`-- sub_directory
")
  end
  
  it "prints summary 1 directory, 1 file" do
    @output.should contain("1 directory, 1 file")
  end
end

describe "FileHierarchyPrinter printing directory with 1 sub directory containing 1 file" do
  before(:each) do
    file = FileEntry.new("/path/sub_directory/file_name")
    sub_dir = DirectoryEntry.new("/path/sub_directory/", [], [ file ])
    dir = DirectoryEntry.new('/path', [ sub_dir ], [])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints file in sub dir indented under sub dir name" do
    @output.should contain(
".
`-- sub_directory
    `-- file_name
")
  end
  
  it "prints summary 1 directory, 1 file" do
    @output.should contain("1 directory, 1 file")
  end
end

describe "FileHierarchyPrinter printing multi-level directory tree containing multiple files" do
  before(:each) do
    file1 = FileEntry.new("/path/sub_directory/file_name1")
    file2 = FileEntry.new("/path/sub_directory/file_name2")
    child_file1 = FileEntry.new("/path/sub_directory/child_file_name1")
    child_file2 = FileEntry.new("/path/sub_directory/child_file_name2")
    grand_child_file2 = FileEntry.new("/path/sub_directory/grand_child_file_name2")

    grand_child_dir1 = DirectoryEntry.new("/path/grand_child_dir1/", [], [])
    grand_child_dir2 = DirectoryEntry.new("/path/grand_child_dir2/", [], [ grand_child_file2 ])
    child_dir = DirectoryEntry.new("/path/child_dir/", 
                                   [ grand_child_dir1, grand_child_dir2 ], 
                                   [ child_file1, child_file2 ])

    dir = DirectoryEntry.new('/path', [ child_dir ], [ file1, file2 ])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints file in sub dir indented under sub dir name" do
    @output.should contain(
".
|-- file_name1
|-- file_name2
`-- child_dir
    |-- child_file_name1
    |-- child_file_name2
    |-- grand_child_dir1
    `-- grand_child_dir2
        `-- grand_child_file_name2
")
  end
  
  it "prints summary 3 directory, 6 files" do
    @output.should contain("3 directories, 5 files")
  end
end

describe "FileHierarchyPrinter printing directory with 2 sub dirs containing 1 file each" do
  before(:each) do
    child_file1 = FileEntry.new("/path/child_dir1/child_file1")
    child_file2 = FileEntry.new("/path/child_dir2/child_file2")
    child_dir1 = DirectoryEntry.new("/path/child_dir1/", [], [ child_file1 ])
    child_dir2 = DirectoryEntry.new("/path/child_dir2/", [], [ child_file2 ])
    dir = DirectoryEntry.new('/path', [ child_dir1, child_dir2 ], [])
    printer = FileHierarchyPrinter.new
    @output = printer.print_to_string(dir)
  end

  it "prints vertical bars to indicate outer directory scope when listing contents of first sub directory" do
    @output.should contain(
".
|-- child_dir1
|   `-- child_file1
`-- child_dir2
    `-- child_file2
")
  end
end
