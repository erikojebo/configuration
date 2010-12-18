require 'path'

class DirectoryEntry
  def initialize(path)
    @path = path
  end

  def directory?
    true
  end

  def file?
    false
  end

  def name
    match = @path.match /.*\/(.*)$/
    match.captures.first
  end

  def path
    @path.with_ending_slash
  end
end
