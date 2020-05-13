#!/bin/bash

for tool in  "github.com/mdempsky/gocode" \
	     "golang.org/x/lint/golint" \
	     "github.com/rogpeppe/godef" \
	     "github.com/kisielk/errcheck" \
	     "golang.org/x/tools/cmd/godoc" \
	     "github.com/zmb3/gogetdoc" \
	     "golang.org/x/tools/cmd/goimports" \
	     "golang.org/x/tools/cmd/gorename" \
	     "golang.org/x/tools/cmd/gomvpkg" \
	     "golang.org/x/tools/cmd/guru" ; do
    go get -u $tool
done

 
 
