require 'path'
require 'file_entry'
require 'directory_entry'

class FileHierarchyReader
  attr_reader :entries

  def read(path)
    files = []
    directories = []
    
    Dir.foreach(path) do |entry|
      next if entry == "." || entry == ".."
      entry_path = File.join(path, entry)
      
      if File.file? entry
        files.push FileEntry.new(entry_path)
      else
        dir = read(entry_path)
        directories.push dir
      end
    end

    directory_entry = DirectoryEntry.new(path, directories, files)
  end
end