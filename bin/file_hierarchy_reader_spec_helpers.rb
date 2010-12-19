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

  stub = Dir.stub!(:foreach)
  stub = Dir.should_receive(:foreach).with(path).at_least(:once)
  fake_entries.each { |e| stub.and_yield(e) }

  directories.each do |entry| 
    entry_path = File.join(path, entry)

    File.stub!(:file?).with(entry_path).and_return(false)
    File.stub!(:directory?).with(entry_path).and_return(true)
  end

  files.each do |entry| 
    entry_path = File.join(path, entry)

    File.stub!(:file?).with(entry_path).and_return(true)
    File.stub!(:directory?).with(entry_path).and_return(false)
  end
end
