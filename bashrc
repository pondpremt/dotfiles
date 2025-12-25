export PATH="$HOME/.cargo/bin:$PATH"
eval "$(/opt/homebrew/bin/brew shellenv)"

# direnv
eval "$(direnv hook bash)"

# starship
eval "$(starship init bash)"
