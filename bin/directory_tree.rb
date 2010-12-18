require 'path'

class FileEntry
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def file?
    true
  end

  def path
    "/path/#{@name}"
  end
end

class DirectoryTree
  attr_reader :entries

  def load_path(path)
    @entries = []

    Dir.foreach(path) do |entry|
      next if entry == "." || entry == ".."
      entry = File.file?(entry) ? FileEntry.new(entry) : DirectoryEntry.new(entry)
      @entries.push entry
    end
  end
end
