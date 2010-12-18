class FileEntry
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def file?
    true
  end

  def directory?
    false
  end

  def path
    "/path/#{@name}"
  end
end
