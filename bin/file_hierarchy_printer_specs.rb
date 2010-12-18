require 'file_hierarchy_printer'
require 'directory_entry'

class StringContainsMatcher
  def initialize(expected_substring)
    @expected_substring = expected_substring
  end

  def matches?(actual)
    @actual = actual
    actual =~ /#{@expected_substring}/
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

describe "FileHierarchyPrinter printing folder with single file" do
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

  it "prints summary with singularized form of file" do
    @output.should contain("1 file")
  end
end
