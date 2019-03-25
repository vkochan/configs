#!/usr/bin/env bash

set -e -u

AUTOBUILD_URL="http://autobuild.buildroot.net"
AUTOBUILD_DIR="${HOME}/br-autobuild"

help() {
    cat <<EOF
get-autobuild: Creates build instance from ${AUTOBUILD_URL} by specified id

Creates build instance in ${AUTOBUILD_DIR}/output/{PKG_NAME}-{ID} by defconfig downloaded from

    ${AUTOBUILD_URL}/results/{ID}/defconfig

where {PKG_NAME} is parsed from build-end.log file, otherwise "unknown-" prefix is used.

All the files from ${AUTOBUILD_URL}/results/{ID} are downloaded to ${AUTOBUILD_DIR}/results/{ID}.

Required options:
    -b ID, --build-id ID
        Build id which results are stored in ${AUTOBUILD_URL}/results/{ID}

Optional options:
    -B PATH, --br-dir PATH
        Path to the buildroot (default is current dir).

     -r, --run
        Run build after it was downloaded and configured.
EOF
}

main() {
    local build_id=""
    local build_dir=""
    local makeargs=""
    local run_build=0
    local pkg_name=""
    local br_dir="."
    local o O opts
    
    # o='hb:B:r'
    # O='help,build-id:br-dir:,run'
    # opts="$(getopt -n "${my_name}" -o "${o}" -l "${O}" -- "${@}")"
    # eval set -- "${opts}"

    while [ ${#} -gt 0 ]; do
        case "$1" in
        (-h|--help)
            help; exit 0
            ;;
        (-b|--build-id)
            build_id="${2}"; shift 2;
            ;;
        (-B|--br-dir)
            br_dir="${2}"; shift 2;
            ;;
        (-r|--run)
            run_build=1; shift 1;
            ;;
        esac
    done
    
    if [ -z "${build_id}" ]; then
        echo "error: missing build id"
        help
        exit 1
    fi
    
    wget -q -R index.htm* -nH -l 1 --recursive --no-parent \
            --directory-prefix=${AUTOBUILD_DIR} \
            ${AUTOBUILD_URL}/results/${build_id}

    pkg_name=$(cat ${AUTOBUILD_DIR}/results/${build_id}/build-end.log | sed -n -r 's|^.*output/build/(.*)/\.stamp_.*$|\1|p')
    if [ -z "${pkg_name}" ]; then
        pkg_name="unknown"
    fi

    build_dir="${AUTOBUILD_DIR}/output/${pkg_name}-${build_id}"
    makeargs="O=${build_dir} -C ${br_dir}"

    mkdir -p ${build_dir}

    make BR2_DEFCONFIG="${AUTOBUILD_DIR}/results/${build_id}/defconfig" ${makeargs} defconfig
    
    if [ ${run_build} -eq 1 ]; then
        make ${makeargs}
    fi
}

my_name="${0##*/}"
main "${@}"
