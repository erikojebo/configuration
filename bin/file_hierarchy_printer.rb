class FileHierarchyPrinter
  def print_to_string(directory_entry)
    directory_summary = "#{directory_entry.total_directory_count} directories"
    file_summary = "#{directory_entry.total_file_count} files"

    if directory_entry.total_directory_count == 1
      directory_summary = "1 directory"
    end
    
    if directory_entry.total_file_count == 1
      file_summary = "1 file"
    end

    listing = directory_entry.entries.size == 0 ? "." : ".\n`-- #{directory_entry.entries.first.name}"

    summary = "#{directory_summary}, #{file_summary}"

    if directory_entry.directories.size == 0 && directory_entry.files.size == 0
      listing = "."
    end

    "#{listing}\n\n#{summary}\n"
  end
end
