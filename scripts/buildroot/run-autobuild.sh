#!/usr/bin/env bash

set -e -u

AUTOBUILD_URL="http://autobuild.buildroot.net"
AUTOBUILD_DIR="${HOME}/br-autobuild"
BR_DIR="."

help() {
    cat <<EOF
run-autobuild: Run build from ${AUTOBUILD_URL} by specified id

Triggers the build into ${AUTOBUILD_DIR}/{ID}/output by defconfig downloaded from

    ${AUTOBUILD_URL}/results/{ID}/defconfig

All the files from ${AUTOBUILD_URL}/results/{ID} are downloaded to ${AUTOBUILD_DIR}/{ID}.

Options:

    -b ID,--build-id ID
        Build id which results are stored in ${AUTOBUILD_URL}/results/{ID}

    -p PREFIX, --prefix PREFIX
        Prepend {ID} with the prefix in the form {PREFIX}-{ID} 

    -B PATH, --br-dir PATH
        Path to the buildroot from where trigger the build
EOF
}

BUILD_ID=""
PREFIX=""

while [ ${#} -gt 0 ]; do
    case "$1" in
    (-h|--help)
        help; exit 0
        ;;
    (-b|--build-id)
        BUILD_ID="${2}"; shift 2;
        ;;
    (-p|--prefix)
        PREFIX="${2}-"; shift 2;
        ;;
    (-B|--br-dir)
        BR_DIR="${2}"; shift 2;
        ;;
    esac
done

if [ -z "${BUILD_ID}" ]; then
    echo "error: missing build id"
    help
    exit 1
fi

BUILD_DIR="${AUTOBUILD_DIR}/${PREFIX}${BUILD_ID}"
MAKEARGS="O=${BUILD_DIR}/output -C ${BR_DIR}"

mkdir -p ${BUILD_DIR}

for f in "branch" "build-end.log" "build-time.log" "config" "defconfig" "gitid" "status" "submitter"; do
    wget ${AUTOBUILD_URL}/results/${BUILD_ID}/${f} --output-document ${BUILD_DIR}/${f}
done

make BR2_DEFCONFIG="${BUILD_DIR}/defconfig" ${MAKEARGS} defconfig &&
        make ${MAKEARGS}
