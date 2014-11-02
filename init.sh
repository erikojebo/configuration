# Make a symlink or the yasnippet directory for js-mode to js2-mode, so that all snippets in one is available
# in the other mode
echo "Creating symlink for the js2-mode yasnippet directory"
ln -s emacs/plugins/yasnippet/snippets/text-mode/js-mode/ emacs/plugins/yasnippet/snippets/text-mode/js2-mode

# Copy all the dot files to the home directory
echo "Copying dot files to the cygwin home directory"
cp .* ~/

