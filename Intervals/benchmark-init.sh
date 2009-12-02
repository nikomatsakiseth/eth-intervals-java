echo Logfile: $1
echo "> date"
date
echo "git show --online | head -1"
git show --oneline | head -1
echo "git diff | cat"
git diff | cat
echo "pwd"
pwd

echo 
echo "Settings:"
echo "JAVAOPTS=${JAVAOPTS}"
echo "NOBUILD=${NOBUILD}"

if [ ! -n "$NOBUILD" ]; then
	ant
fi
