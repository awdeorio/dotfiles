# programmable bash_completion file for the main OpenOffice.org applications

_oobase2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.odb'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oobase2 -o filenames -o dirnames oobase2

_oofromtemplate2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.dot'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.stc'
				compgen -G "${cur}*" -X '!*.xlt'
				compgen -G "${cur}*" -X '!*.sti'
				compgen -G "${cur}*" -X '!*.pot'
				compgen -G "${cur}*" -X '!*.std'
				compgen -G "${cur}*" -X '!*.stw'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oofromtemplate2 -o filenames -o dirnames oofromtemplate2

_oodraw2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.sxd'
				compgen -G "${cur}*" -X '!*.std'
				compgen -G "${cur}*" -X '!*.dxf'
				compgen -G "${cur}*" -X '!*.emf'
				compgen -G "${cur}*" -X '!*.eps'
				compgen -G "${cur}*" -X '!*.met'
				compgen -G "${cur}*" -X '!*.pct'
				compgen -G "${cur}*" -X '!*.sgf'
				compgen -G "${cur}*" -X '!*.sgv'
				compgen -G "${cur}*" -X '!*.sda'
				compgen -G "${cur}*" -X '!*.sdd'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.svm'
				compgen -G "${cur}*" -X '!*.wmf'
				compgen -G "${cur}*" -X '!*.bmp'
				compgen -G "${cur}*" -X '!*.gif'
				compgen -G "${cur}*" -X '!*.jpg'
				compgen -G "${cur}*" -X '!*.jpeg'
				compgen -G "${cur}*" -X '!*.jfif'
				compgen -G "${cur}*" -X '!*.fif'
				compgen -G "${cur}*" -X '!*.jpe'
				compgen -G "${cur}*" -X '!*.pcd'
				compgen -G "${cur}*" -X '!*.pcx'
				compgen -G "${cur}*" -X '!*.pgm'
				compgen -G "${cur}*" -X '!*.png'
				compgen -G "${cur}*" -X '!*.ppm'
				compgen -G "${cur}*" -X '!*.psd'
				compgen -G "${cur}*" -X '!*.ras'
				compgen -G "${cur}*" -X '!*.tga'
				compgen -G "${cur}*" -X '!*.tif'
				compgen -G "${cur}*" -X '!*.tiff'
				compgen -G "${cur}*" -X '!*.xbm'
				compgen -G "${cur}*" -X '!*.xpm'
				compgen -G "${cur}*" -X '!*.odg'
				compgen -G "${cur}*" -X '!*.otg'
				compgen -G "${cur}*" -X '!*.odc'
				compgen -G "${cur}*" -X '!*.odi'
				compgen -G "${cur}*" -X '!*.sds'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oodraw2 -o filenames -o dirnames oodraw2

_oocalc2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.sxc'
				compgen -G "${cur}*" -X '!*.stc'
				compgen -G "${cur}*" -X '!*.dif'
				compgen -G "${cur}*" -X '!*.dbf'
				compgen -G "${cur}*" -X '!*.xls'
				compgen -G "${cur}*" -X '!*.xlw'
				compgen -G "${cur}*" -X '!*.xlt'
				compgen -G "${cur}*" -X '!*.rtf'
				compgen -G "${cur}*" -X '!*.sdc'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.slk'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.htm'
				compgen -G "${cur}*" -X '!*.html'
				compgen -G "${cur}*" -X '!*.wk1'
				compgen -G "${cur}*" -X '!*.wks'
				compgen -G "${cur}*" -X '!*.123'
				compgen -G "${cur}*" -X '!*.xml'
				compgen -G "${cur}*" -X '!*.ods'
				compgen -G "${cur}*" -X '!*.ots'
				compgen -G "${cur}*" -X '!*.csv'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oocalc2 -o filenames -o dirnames oocalc2

_oomath2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.sxm'
				compgen -G "${cur}*" -X '!*.smf'
				compgen -G "${cur}*" -X '!*.mml'
				compgen -G "${cur}*" -X '!*.odf'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oomath2 -o filenames -o dirnames oomath2

_ooweb2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.htm'
				compgen -G "${cur}*" -X '!*.html'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.oth'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _ooweb2 -o filenames -o dirnames ooweb2

_ooffice2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.sxd'
				compgen -G "${cur}*" -X '!*.std'
				compgen -G "${cur}*" -X '!*.dxf'
				compgen -G "${cur}*" -X '!*.emf'
				compgen -G "${cur}*" -X '!*.eps'
				compgen -G "${cur}*" -X '!*.met'
				compgen -G "${cur}*" -X '!*.pct'
				compgen -G "${cur}*" -X '!*.sgf'
				compgen -G "${cur}*" -X '!*.sgv'
				compgen -G "${cur}*" -X '!*.sda'
				compgen -G "${cur}*" -X '!*.sdd'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.svm'
				compgen -G "${cur}*" -X '!*.wmf'
				compgen -G "${cur}*" -X '!*.bmp'
				compgen -G "${cur}*" -X '!*.gif'
				compgen -G "${cur}*" -X '!*.jpg'
				compgen -G "${cur}*" -X '!*.jpeg'
				compgen -G "${cur}*" -X '!*.jfif'
				compgen -G "${cur}*" -X '!*.fif'
				compgen -G "${cur}*" -X '!*.jpe'
				compgen -G "${cur}*" -X '!*.pcd'
				compgen -G "${cur}*" -X '!*.pcx'
				compgen -G "${cur}*" -X '!*.pgm'
				compgen -G "${cur}*" -X '!*.png'
				compgen -G "${cur}*" -X '!*.ppm'
				compgen -G "${cur}*" -X '!*.psd'
				compgen -G "${cur}*" -X '!*.ras'
				compgen -G "${cur}*" -X '!*.tga'
				compgen -G "${cur}*" -X '!*.tif'
				compgen -G "${cur}*" -X '!*.tiff'
				compgen -G "${cur}*" -X '!*.xbm'
				compgen -G "${cur}*" -X '!*.xpm'
				compgen -G "${cur}*" -X '!*.odg'
				compgen -G "${cur}*" -X '!*.otg'
				compgen -G "${cur}*" -X '!*.odc'
				compgen -G "${cur}*" -X '!*.odi'
				compgen -G "${cur}*" -X '!*.sds'
				compgen -G "${cur}*" -X '!*.doc'
				compgen -G "${cur}*" -X '!*.dot'
				compgen -G "${cur}*" -X '!*.rtf'
				compgen -G "${cur}*" -X '!*.sxw'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.sdw'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.htm?'
				compgen -G "${cur}*" -X '!*.xml'
				compgen -G "${cur}*" -X '!*.wp'
				compgen -G "${cur}*" -X '!*.wpd'
				compgen -G "${cur}*" -X '!*.odt'
				compgen -G "${cur}*" -X '!*.ott'
				compgen -G "${cur}*" -X '!*.sxm'
				compgen -G "${cur}*" -X '!*.smf'
				compgen -G "${cur}*" -X '!*.mml'
				compgen -G "${cur}*" -X '!*.odf'
				compgen -G "${cur}*" -X '!*.sxi'
				compgen -G "${cur}*" -X '!*.sti'
				compgen -G "${cur}*" -X '!*.ppt'
				compgen -G "${cur}*" -X '!*.pps'
				compgen -G "${cur}*" -X '!*.pot'
				compgen -G "${cur}*" -X '!*.sxd'
				compgen -G "${cur}*" -X '!*.sda'
				compgen -G "${cur}*" -X '!*.sdd'
				compgen -G "${cur}*" -X '!*.sdp'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.cgm'
				compgen -G "${cur}*" -X '!*.odp'
				compgen -G "${cur}*" -X '!*.otp'
				compgen -G "${cur}*" -X '!*.odb'
				compgen -G "${cur}*" -X '!*.sxc'
				compgen -G "${cur}*" -X '!*.stc'
				compgen -G "${cur}*" -X '!*.dif'
				compgen -G "${cur}*" -X '!*.dbf'
				compgen -G "${cur}*" -X '!*.xls'
				compgen -G "${cur}*" -X '!*.xlw'
				compgen -G "${cur}*" -X '!*.xlt'
				compgen -G "${cur}*" -X '!*.rtf'
				compgen -G "${cur}*" -X '!*.sdc'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.slk'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.htm'
				compgen -G "${cur}*" -X '!*.html'
				compgen -G "${cur}*" -X '!*.wk1'
				compgen -G "${cur}*" -X '!*.wks'
				compgen -G "${cur}*" -X '!*.123'
				compgen -G "${cur}*" -X '!*.xml'
				compgen -G "${cur}*" -X '!*.ods'
				compgen -G "${cur}*" -X '!*.ots'
				compgen -G "${cur}*" -X '!*.csv'
				compgen -G "${cur}*" -X '!*.sxg'
				compgen -G "${cur}*" -X '!*.xgl'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.odm'
				compgen -G "${cur}*" -X '!*.sgl'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.dot'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.stc'
				compgen -G "${cur}*" -X '!*.xlt'
				compgen -G "${cur}*" -X '!*.sti'
				compgen -G "${cur}*" -X '!*.pot'
				compgen -G "${cur}*" -X '!*.std'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.htm'
				compgen -G "${cur}*" -X '!*.html'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.oth'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _ooffice2 -o filenames -o dirnames ooffice2

_oowriter2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.doc'
				compgen -G "${cur}*" -X '!*.dot'
				compgen -G "${cur}*" -X '!*.rtf'
				compgen -G "${cur}*" -X '!*.sxw'
				compgen -G "${cur}*" -X '!*.stw'
				compgen -G "${cur}*" -X '!*.sdw'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.txt'
				compgen -G "${cur}*" -X '!*.htm?'
				compgen -G "${cur}*" -X '!*.xml'
				compgen -G "${cur}*" -X '!*.wp'
				compgen -G "${cur}*" -X '!*.wpd'
				compgen -G "${cur}*" -X '!*.odt'
				compgen -G "${cur}*" -X '!*.ott'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _oowriter2 -o filenames -o dirnames oowriter2

_ooimpress2() {
	local prev cur options IFS=$'\t\n'

	COMPREPLY=()
	cur=${COMP_WORDS[COMP_CWORD]}
	prev=${COMP_WORDS[COMP_CWORD-1]}
	case $prev in
		*)
			COMPREPLY=( $(
				compgen -G "${cur}*" -X '!*.sxi'
				compgen -G "${cur}*" -X '!*.sti'
				compgen -G "${cur}*" -X '!*.ppt'
				compgen -G "${cur}*" -X '!*.pps'
				compgen -G "${cur}*" -X '!*.pot'
				compgen -G "${cur}*" -X '!*.sxd'
				compgen -G "${cur}*" -X '!*.sda'
				compgen -G "${cur}*" -X '!*.sdd'
				compgen -G "${cur}*" -X '!*.sdp'
				compgen -G "${cur}*" -X '!*.vor'
				compgen -G "${cur}*" -X '!*.cgm'
				compgen -G "${cur}*" -X '!*.odp'
				compgen -G "${cur}*" -X '!*.otp'
			) )
		;;
	esac

	return 0

}
complete -d -X '.[^./]*' -F _ooimpress2 -o filenames -o dirnames ooimpress2

