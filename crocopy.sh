if [ "$#" -lt 1 ]; then
    echo "Nothing to copy."
    exit 1
fi 
        
scp "$@" gisv70@gemini:/mnt/ScratchPool/dario10/FINIS

