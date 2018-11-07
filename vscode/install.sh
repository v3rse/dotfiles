#!/bin/bash
while read p; do code --install-extension $p; done < ./extensions-list.txt

