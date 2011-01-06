-module(SKEL_SUITE).
-compile(export_all).

all() -> [succeeds,noisy,lazy,fails,asplodes].

succeeds(_) -> ok.
noisy(_) -> {comment,"Hello!"}.
lazy(_) -> {skip,"I don't feel like working."}.
fails(_) -> exit("failed").
asplodes(_) -> 1 / 0.
