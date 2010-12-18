class FileEntry
  attr_reader :path

  def initialize(path)
    @path = path
  end

  def file?
    true
  end

  def directory?
    false
  end

  def name
    path_with_leading_slash = "/" + path
    match = path_with_leading_slash.match(/.*\/(.*)/)
    match.captures.first
  end
end
