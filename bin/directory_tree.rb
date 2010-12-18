require 'path'
require 'file_entry'
require 'directory_entry'

class FileHierarchyReader
  attr_reader :entries

  def load_path(path)
    @entries = []

    Dir.foreach(path) do |entry|
      next if entry == "." || entry == ".."
      entry_path = File.join(path, entry)
      entry = File.file?(entry) ? FileEntry.new(entry) : DirectoryEntry.new(entry_path)
      @entries.push entry
    end
  end
end
