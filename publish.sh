#!/bin/bash

#get highest tag number
VERSION=`git describe --abbrev=0 --tags`

#replace . with space so can split into an array
VERSION_BITS=(${VERSION//./ })

#get number parts and increase last one by 1
VNUM1=${VERSION_BITS[0]}
VNUM2=${VERSION_BITS[1]}
VNUM3=${VERSION_BITS[2]}
VNUM2=$((VNUM2+1))

#create new tag
NEW_TAG="$VNUM1.$VNUM2.$VNUM3"

echo "Updating $VERSION to $NEW_TAG"

# Create a release and store the image and corresponding scene in the repository
mkdir ./meta/releases/$NEW_TAG
cp meta/scenes/main.json ./meta/releases/$NEW_TAG/$NEW_TAG.json

# Render the image for this version of the code base
dotnet run --project illuminate.console -- ./meta/releases/$NEW_TAG/$NEW_TAG.json ./meta/releases/$NEW_TAG/$NEW_TAG.jpeg

#commit the files
git add .
git commit -am $NEW_TAG
git push

git tag -a $NEW_TAG -m $NEW_TAG
git push --tags