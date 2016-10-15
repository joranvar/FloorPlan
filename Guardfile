# Runs the command and prints a notification
def execute(cmd)
  if system(cmd)
    n 'Build succeeded', 'stack', :success
  else
    n 'Build failed', 'stack', :failed
  end
end

def run_all_tests m
  print m
  execute %{
    stack test
    stack exec FloorPlan-exe
  }
end

guard :shell do
  watch(%r{^[^#]*\.cabal$})          { |m| run_all_tests m[0] }
  watch(%r{^[^#]*\.hs$})             { |m| run_all_tests m[0] }
end
