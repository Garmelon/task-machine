#!/bin/bash
find src app -name "*.hs" -exec stylish-haskell -i {} \;
hlint src app
