import 'Rakefile.helpers'
import 'Rakefile.build'
import 'Rakefile.test'

task :default => ["build:compile"]

namespace :debug do
  desc 'open a console loaded with the apps in this library'
  task :console => ["build:compile"] do
    sh 'erl', '-sname', 'console'
  end
  
  desc 'lists workers'
  task :workers do
    puts workers
  end
end

namespace :run do
  desc 'starts workers'
  task :start_workers, [:number] do |t,args|
    raise "workers already started" if workers.length > 0
    workers = args.number.to_i
    if workers > 0
      (0 ... workers).each do |x|
        sh 'scripts/start_node', sprintf('worker_%05d',x)
      end
    else
      puts 'Please specify how many workers to start...'
      exit 1
    end
  end
  
  desc 'kills workers'
  task :kill_workers do
    workers.each do |w|
      sh "scripts/stop_node", w
    end
  end
end

namespace :manage do
  desc 'create a new app in the library (skel is optional)'
  task :new_app, [:dir,:skel] do |t,args|
    args.with_defaults(:skel => 'skel')
    unless args[:dir]
      puts 'Please specify a name for the new application...'
      exit 1
    end
    copy_skel args[:skel], args[:dir]
  end
end
