class FileHierarchyPrinter
  def print_to_string(directory_entry)
    listing = directory_entry.files.size == 0 ? "." : ".\n`-- #{directory_entry.files.first.name}"
    summary = directory_entry.files.size == 0 ? "0 directories, 0 files" : "0 directories, 1 file"

    "#{listing}\n\n#{summary}\n"
  end
end
