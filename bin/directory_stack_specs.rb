# To run these specs:
# > gem install rspec
# > rspec filename.rb

require 'directory_stack'

describe "Directory stack with relative initial directory" do
  before(:each) do
    @stack = DirectoryStack.new("relative")
  end

  it "has relative current path when created" do
    @stack.current.should == "relative/"
  end
end


describe "Directory stack with initial directory" do
  before(:each) do
    @stack = DirectoryStack.new("/some/dir/")
  end

  it "is in initial directory when created" do
    @stack.current.should == "/some/dir/"
  end

  it "has depth zero when created" do
    @stack.depth.should == 0
  end

  it "has depth one after pushing one directory" do
    @stack.push "child"
    @stack.depth.should == 1
  end

  it "adds pushed directory to current path" do
    @stack.push "child/"
    @stack.current.should == "/some/dir/child/"
  end

  it "appends trailing slash" do
    @stack.push "child"
    @stack.current.should == "/some/dir/child/"
  end

  it "accumulates pushed directories to current path" do
    @stack.push "child"
    @stack.push "grand_child"
    @stack.current.should == "/some/dir/child/grand_child/"
  end
end

describe "Directory stack without initial directory" do
  before(:each) do
    @stack = DirectoryStack.new
  end

  it "has no current path when created" do
    @stack.current.should == ""
  end

  it "has depth zero when created" do
    @stack.depth.should == 0
  end

  it "has depth zero after pushing initial directory" do
    @stack.push "initial"
    @stack.depth.should == 0
  end

  it "sets current path to initial directory" do
    @stack.push "/initial"
    @stack.current.should == "/initial/"
  end
end
