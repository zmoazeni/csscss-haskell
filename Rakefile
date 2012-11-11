files = FileList["src/*.hs"]
compile_cmd = "ghc -outputdir bin -isrc -Wall #{ENV['GHC_OPTS']}".strip

files.each do |f|
  o_file = f.pathmap("%{^src/bin}X.o")
  file o_file => f do
    sh "#{compile_cmd} #{f}"
  end
end

file "bin/Main.o" => files.dup.exclude(/Main\.hs/).pathmap("%{^src/bin}X.o")

desc "Compile srcs"
task :compile => files.pathmap("%{^src/bin}X.o")
task :default => :compile

desc "Run all the specs"
task :spec do
  sh "runghc -isrc -ispec spec/Suite.hs"
end

namespace :clean do
  desc "Clean the compiled objects"
  task :src do
    sh "rm -rf ./bin ./src/Main"
  end

  desc "Clean compiled dist"
  task :dist do
    sh "rm -rf dist"
  end

  desc "Clean cabal-dev"
  task :cabaldev do
    sh "rm -rf cabal-dev"
  end

  desc "Clean both compiled objects and db"
  task :all => [:src, :dist, :cabaldev]
end
desc "Clean the compiled objects, same as clean:src"
task :clean => "clean:src"
