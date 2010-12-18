require 'path'
require 'file_entry'

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
    @path.with_ending_slash
  end
end
