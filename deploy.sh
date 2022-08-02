#!/usr/bin/env sh

cd Deploy
rm -rf .git
git init --initial-branch=main
git config user.name "sync"
git config user.email "sync@nothing"
git remote add origin $REMOTE_URL
git add .
git commit -m "sync files from origin repo"
git push -u origin main --force
