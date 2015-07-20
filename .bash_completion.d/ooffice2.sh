# Programmable bash_completion file for the main OpenOffice.org applications
# It is based on /etc/profile.d/complete.bash from SUSE Linux 10.1

_def=; _dir=; _file=; _nosp=
if complete -o default _nullcommand &> /dev/null ; then
    _def="-o default"
    _dir="-o dirnames"
    _file="-o filenames"
fi
_minusdd="-d ${_dir}"
_minusdf="-d ${_file}"
if complete -o nospace _nullcommand &> /dev/null ; then
    _nosp="-o nospace"
    _minusdd="${_nosp} ${_dir}"
    _minusdf="${_nosp} ${_dir}"
fi
complete -r _nullcommand &> /dev/null

# General OOo expanding shell function
_ooexp_ ()
{
    # bash `complete' is broken because you can not combine
    # -d, -f, and -X pattern without missing directories.
    local c=${COMP_WORDS[COMP_CWORD]}
    local a="${COMP_LINE}"
    local e s g=0 cd dc t=""
    local IFS

    shopt -q extglob && g=1
    test $g -eq 0 && shopt -s extglob
    # Don't be fooled by the bash parser if extglob is off by default
    cd='*-?(c)d*'
    dc='*-d?(c)*'

    case "${1##*/}" in
    oobase2)		e='!*.+(odb)' ;;
    oofromtemplate2)		e='!*.+(stw|dot|vor|stc|xlt|sti|pot|std|stw)' ;;
    oodraw2)		e='!*.+(sxd|std|dxf|emf|eps|met|pct|sgf|sgv|sda|sdd|vor|svm|wmf|bmp|gif|jpg|jpeg|jfif|fif|jpe|pcd|pcx|pgm|png|ppm|psd|ras|tga|tif|tiff|xbm|xpm|odg|otg|odc|odi|sds)' ;;
    oocalc2)		e='!*.+(sxc|stc|dif|dbf|xls|xlw|xlt|rtf|sdc|vor|slk|txt|htm|html|wk1|wks|123|xml|ods|ots|csv)' ;;
    oomath2)		e='!*.+(sxm|smf|mml|odf)' ;;
    ooweb2)		e='!*.+(htm|html|stw|txt|vor|oth)' ;;
    ooffice2)		e='!*.+(sxd|std|dxf|emf|eps|met|pct|sgf|sgv|sda|sdd|vor|svm|wmf|bmp|gif|jpg|jpeg|jfif|fif|jpe|pcd|pcx|pgm|png|ppm|psd|ras|tga|tif|tiff|xbm|xpm|odg|otg|odc|odi|sds|doc|dot|rtf|sxw|stw|sdw|vor|txt|htm?|xml|wp|wpd|odt|ott|sxm|smf|mml|odf|sxi|sti|ppt|pps|pot|sxd|sda|sdd|sdp|vor|cgm|odp|otp|odb|sxc|stc|dif|dbf|xls|xlw|xlt|rtf|sdc|vor|slk|txt|htm|html|wk1|wks|123|xml|ods|ots|csv|sxg|xgl|txt|odm|sgl|stw|dot|vor|stc|xlt|sti|pot|std|stw|htm|html|stw|txt|vor|oth)' ;;
    oowriter2)		e='!*.+(doc|dot|rtf|sxw|stw|sdw|vor|txt|htm?|xml|wp|wpd|odt|ott)' ;;
    ooimpress2)		e='!*.+(sxi|sti|ppt|pps|pot|sxd|sda|sdd|sdp|vor|cgm|odp|otp)' ;;
    *)			e='!*'
    esac

    case "$(complete -p ${1##*/} 2> /dev/null)" in
	*-d*)	;;
	*) s="-S/"
    esac

    IFS='
'
    case "$c" in
    \$\(*\))	   eval COMPREPLY=\(${c}\) ;;
    \$\(*)		COMPREPLY=($(compgen -c -P '$(' -S ')'  -- ${c#??}))	;;
    \`*\`)	   eval COMPREPLY=\(${c}\) ;;
    \`*)		COMPREPLY=($(compgen -c -P '\`' -S '\`' -- ${c#?}))	;;
    \$\{*\})	   eval COMPREPLY=\(${c}\) ;;
    \$\{*)		COMPREPLY=($(compgen -v -P '${' -S '}'  -- ${c#??}))	;;
    \$*)		COMPREPLY=($(compgen -v -P '$'          -- ${c#?}))	;;
    \~*/*)		COMPREPLY=($(compgen -f -X "$e"         -- ${c}))	;;
    \~*)		COMPREPLY=($(compgen -u ${s}	 	-- ${c}))	;;
    *@*)		COMPREPLY=($(compgen -A hostname -P '@' -S ':' -- ${c#*@})) ;;
    *[*?[]*)		COMPREPLY=($(compgen -G "${c}"))			;;
    *[?*+\!@]\(*\)*)
	if test $g -eq 0 ; then
			COMPREPLY=($(compgen -f -X "$e" -- $c))
			test $g -eq 0 && shopt -u extglob
			return
	fi
			COMPREPLY=($(compgen -G "${c}"))			;;
    *)
	if test "$c" = ".." ; then
			COMPREPLY=($(compgen -d -X "$e" -S / ${_nosp} -- $c))
	else
			for s in $(compgen -f -X "$e" -- $c) ; do
			    if test -d $s ; then
				COMPREPLY=(${COMPREPLY[@]} $(compgen -f -X "$e" -S / -- $s))
			    elif test -z "$t" ; then
				COMPREPLY=(${COMPREPLY[@]} $s)
			    else
				case "$(file -b $s 2> /dev/null)" in
				$t) COMPREPLY=(${COMPREPLY[@]} $s)		;;
				esac
			    fi
			done
	fi									;;
    esac
    test $g -eq 0 && shopt -u extglob
}


complete -d -X '.[^./]*' -F _ooexp_ ${_file} \
					oobase2 \
					oofromtemplate2 \
					oodraw2 \
					oocalc2 \
					oomath2 \
					ooweb2 \
					ooffice2 \
					oowriter2 \
					ooimpress2

unset _def _dir _file _nosp _minusdd _minusdf
