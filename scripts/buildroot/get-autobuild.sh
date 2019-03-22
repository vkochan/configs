#!/usr/bin/env bash

set -e -u

AUTOBUILD_URL="http://autobuild.buildroot.net"
AUTOBUILD_DIR="${HOME}/br-autobuild"

help() {
    cat <<EOF
get-autobuild: Creates build instance from ${AUTOBUILD_URL} by specified id

Creates build instance in ${AUTOBUILD_DIR}/{ID}/output by defconfig downloaded from

    ${AUTOBUILD_URL}/results/{ID}/defconfig

All the files from ${AUTOBUILD_URL}/results/{ID} are downloaded to ${AUTOBUILD_DIR}/{ID}.

Required options:
    -b ID, --build-id ID
        Build id which results are stored in ${AUTOBUILD_URL}/results/{ID}

Optional options:
    -p PREFIX, --prefix PREFIX
        Prepend {ID} with the prefix in the form {PREFIX}-{ID} 

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
    local br_dir="."
    local prefix=""
    
    while [ ${#} -gt 0 ]; do
        case "$1" in
        (-h|--help)
            help; exit 0
            ;;
        (-b|--build-id)
            build_id="${2}"; shift 2;
            ;;
        (-p|--prefix)
            prefix="${2}-"; shift 2;
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
    
    build_dir="${AUTOBUILD_DIR}/${prefix}${build_id}"
    makeargs="O=${build_dir}/output -C ${br_dir}"
    
    mkdir -p ${build_dir}
    
    for f in "branch" "build-end.log" "build-time.log" "config" "defconfig" "gitid" "status" "submitter"; do
        wget ${AUTOBUILD_URL}/results/${build_id}/${f} --output-document ${build_dir}/${f}
    done
    
    make BR2_DEFCONFIG="${build_dir}/defconfig" ${makeargs} defconfig
    
    if [ ${run_build} -eq 1 ]; then
        make ${makeargs}
    fi
}

main ${@}
