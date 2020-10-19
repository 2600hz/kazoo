DEPS = apns fcm lager

dep_apns = git https://github.com/2600hz/erlang-apns4erl.git 20668a1f5fde5a7afd195d03a5cd30825786f2c5 # latest commit SHA to 2600hz branch

dep_fcm = git https://github.com/2600hz/erlang-fcm.git b2f68a4c6f0f59475597a35e2dc9be13d9ba2910
# Firebase cloud messaging
# used by pusher

# Need to move gcm to proper dep here
