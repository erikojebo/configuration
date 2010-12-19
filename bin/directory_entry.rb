require File.dirname(__FILE__) + '/path'
require File.dirname(__FILE__) + '/file_entry'

class DirectoryEntry

  attr_reader :files, :directories

  def initialize(path, directories = [], files = [])
    @path = path
    @files = files
    @directories = directories
  end

  def directory?
    true
  end

  def file?
    false
  end

  def entries
    directories.concat files
  end

  def name
    # Make sure the path to match agains starts with a slash so that
    # there is always at least two slashes between which to extract
    # the name
    path_with_leading_slash = '/' + path

    # Extract text between last two forward slashes
    match = path_with_leading_slash.match /.*\/(.*)\/$/
    match.captures.first
  end

  def path
    @path.with_trailing_slash
  end

  def total_directory_count
    directories_in_sub_dirs = directories.map{|d| d.total_directory_count}.inject{|sum, x| sum + x}

    if (directories_in_sub_dirs)
      return directories.size + directories_in_sub_dirs
    end

    directories.size
  end

  def total_file_count
    files_in_sub_dirs = directories.map{|d| d.total_file_count}.inject{|sum, x| sum + x}

    if (files_in_sub_dirs)
      return files.size + files_in_sub_dirs
    end

    files.size
  end
end
