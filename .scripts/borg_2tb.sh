#!/bin/bash
set -eE -o pipefail

# external drive label
DEVICE_LABEL="BACKUP_2TB"
BACKUP_LABEL="/home/"

# sleep time when external drive isn't plugged in yet
# sleep for 5 seconds for up to 60 times (= 5 minutes total)
DEVICE_SLEEP=5
DEVICE_SLEEP_COUNT=60

# repository path (relative to drive mountpoint)
REPOSITORY="Borg"

# be verbose, otherwise output nothing (except errors)
BORG_CREATE_PARAMS=( -v --stats --show-rc )
BORG_DELETE_PARAMS=( -v --stats --show-rc )
BORG_PRUNE_PARAMS=( -v --list --stats --show-rc )
if tty --quiet; then
    BORG_CREATE_PARAMS+=( --progress )
    BORG_DELETE_PARAMS+=( --progress )
fi
if [ "$1" == "-q" ] || [ "$1" == "--quiet" ]; then
    BORG_CREATE_PARAMS=()
    BORG_DELETE_PARAMS=()
    BORG_PRUNE_PARAMS=()
fi

# check for external drive
if [ ! -b "/dev/disk/by-label/$DEVICE_LABEL" ]; then
    SUMMARY="Borg Backup"
    BODY="To start your Borg Backup \"$BACKUP_LABEL\", you must plug in your \"$DEVICE_LABEL\" drive!"
    notify-send --app-name="borg-notify" --icon="borg" --urgency="normal" --expire-time="$(($DEVICE_SLEEP * 1000))" "$SUMMARY" "$BODY"

    echo "Waiting for external drive: $DEVICE_LABEL"

    for (( i=1; i <= $DEVICE_SLEEP_COUNT; i++ )); do
        sleep $DEVICE_SLEEP

        if [ -b "/dev/disk/by-label/$DEVICE_LABEL" ]; then
            sleep $DEVICE_SLEEP
            break
        fi
    done

    [ -b "/dev/disk/by-label/$DEVICE_LABEL" ] || exit 75
fi

MOUNTPOINT="$(findmnt --first-only --noheadings --output=target --source "/dev/disk/by-label/$DEVICE_LABEL" || true)"
if [ -z "$MOUNTPOINT" ]; then
    echo "Waiting for auto mount: $DEVICE_LABEL"

    for (( i=1; i <= $MOUNT_SLEEP_COUNT; i++ )); do
        sleep $MOUNT_SLEEP

        MOUNTPOINT="$(findmnt --first-only --noheadings --output=target --source "/dev/disk/by-label/$DEVICE_LABEL" || true)"
        if [ -z "$MOUNTPOINT" ]; then
            sleep $MOUNT_SLEEP
            break;
        fi
    done

    if [ -z "$MOUNTPOINT" ]; then
        echo "External drive is not mounted: $DEVICE_LABEL" >&2
        exit 1
    fi
fi

if [ ! -d "$MOUNTPOINT" ]; then
    echo "External drive \"$DEVICE_LABEL\" has a invalid mount point: $MOUNTPOINT" >&2
    exit 1
fi

echo "External drive \"$DEVICE_LABEL\" mounted: $MOUNTPOINT"

REPOSITORY="$MOUNTPOINT/$REPOSITORY"
trap "sync -f \"$REPOSITORY\"; sync -f \"$REPOSITORY\"" INT TERM EXIT

# prepare repository
if [ ! -e "$REPOSITORY" ]; then
    mkdir "$REPOSITORY"
    borg init --encryption=repokey "$REPOSITORY"
fi
if [ ! -d "$REPOSITORY" ]; then
    echo "Invalid repository: $REPOSITORY" >&2
    exit 1
fi

# create backup (ignore warnings)
BORG_CREATE_STATUS=0
borg create "${BORG_CREATE_PARAMS[@]}" \
    --compression=lz4 \
    --exclude-caches --one-file-system \
    "$REPOSITORY"::'{hostname}-{now:%Y-%m-%dT%H:%M:%S}' \
    "$HOME" \
    --exclude '$HOME/.cache' \
    --exclude '$HOME/.local/share/Trash' \
    --exclude '$HOME/.thumbnails' \
    || { BORG_CREATE_STATUS=$?; true; }

[ $BORG_CREATE_STATUS -lt 2 ] || exit $BORG_CREATE_STATUS

# remove checkpoints (ignore warnings)
BORG_DELETE_STATUS=0
if [ $BORG_CREATE_STATUS -eq 0 ]; then
    BORG_CHECKPOINTS="$(borg list --short "$REPOSITORY" | grep -E '\.checkpoint(\.[0-9]+)?$' || true)"
    if [ -n "$BORG_CHECKPOINTS" ]; then
        xargs --max-args 1 --no-run-if-empty -I "%ARCHIVE%" -- \
            borg delete "${BORG_DELETE_PARAMS[@]}" "$REPOSITORY"::"%ARCHIVE%" \
            <<< "$BORG_CHECKPOINTS" \
            || { BORG_DELETE_STATUS=$?; true; }

        [ $BORG_DELETE_STATUS -lt 2 ] || exit $BORG_DELETE_STATUS
    fi
fi

# prune old backups (ignore warnings)
# keep everything within two days + 12 daily backups (= 2 weeks),
# 26 weekly backups (= 6 months), 24 monthly backups (= 2 years),
# 10 yearly backups
BORG_PRUNE_STATUS=0
borg prune "${BORG_PRUNE_PARAMS[@]}" \
    "$REPOSITORY" --prefix '{hostname}-' \
    --keep-within=2d --keep-daily=12 \
    --keep-weekly=26 --keep-monthly=24 \
    --keep-yearly=10 \
    || { BORG_PRUNE_STATUS=$?; true; }

[ $BORG_PRUNE_STATUS -lt 2 ] || exit $BORG_PRUNE_STATUS

if [ $BORG_CREATE_STATUS -eq 1 ] || [ $BORG_DELETE_STATUS -eq 1 ] || [ $BORG_PRUNE_STATUS -eq 1 ]; then
    exit 254
fi
