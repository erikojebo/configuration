require File.dirname(__FILE__) + '/path'
require File.dirname(__FILE__) + '/directory_entry'

class FileHierarchyReader
  attr_reader :entries

  def read(path)
    files = []
    directories = []
    
    Dir.foreach(path) do |entry|
      next if entry == "." || entry == ".."
      entry_path = File.join(path, entry)
      
      if File.file? entry_path
        files.push FileEntry.new(entry_path)
      elsif File.directory? entry_path
        dir = read(entry_path)
        directories.push dir
      else
        fail "Neither file nor directory: #{entry}"
      end
    end

    directory_entry = DirectoryEntry.new(path, directories, files)
  end
end
