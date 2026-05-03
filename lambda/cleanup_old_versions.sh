#!/bin/bash

# Script to delete all but the last 10 versions of a Lambda function
# Usage: ./cleanup-lambda-versions.sh <function-name> [region]

# example usage:
# aws lambda list-functions --region eu-central-1 --query 'Functions[].FunctionName' --output text | tr '\t' '\n' | xargs -I{} ./cleanup_old_versions.sh {} eu-central-1

FUNCTION_NAME="$1"

REGION="${2:-us-east-1}"  # Default to us-east-1 if no region specified

if [ -z "$FUNCTION_NAME" ]; then
    echo "Usage: $0 <function-name> [region]"
    echo "Example: $0 my-function us-west-2"
    exit 1
fi

echo "Cleaning up versions for function: $FUNCTION_NAME in region: $REGION"

# Get all versions except $LATEST, sort by version number, keep only the version numbers
VERSIONS=$(aws lambda list-versions-by-function \
    --function-name "$FUNCTION_NAME" \
    --region "$REGION" \
    --query 'Versions[?Version!=`$LATEST`].Version' \
    --output text | tr '\t' '\n' | sort -n)

# Convert to array
VERSION_ARRAY=($VERSIONS)
TOTAL_VERSIONS=${#VERSION_ARRAY[@]}

echo "Found $TOTAL_VERSIONS versions (excluding \$LATEST)"

# If we have more than 10 versions, delete the older ones
if [ $TOTAL_VERSIONS -gt 10 ]; then
    VERSIONS_TO_DELETE=$((TOTAL_VERSIONS - 10))
    echo "Deleting $VERSIONS_TO_DELETE old versions..."
    
    # Delete the first (oldest) versions, keeping the last 10
    for ((i=0; i<$VERSIONS_TO_DELETE; i++)); do
        VERSION="${VERSION_ARRAY[$i]}"
        echo "Deleting version: $VERSION"
        
        aws lambda delete-function \
            --function-name "$FUNCTION_NAME" \
            --qualifier "$VERSION" \
            --region "$REGION"
        
        if [ $? -eq 0 ]; then
            echo "✓ Successfully deleted version $VERSION"
        else
            echo "✗ Failed to delete version $VERSION"
        fi
    done
    
    echo "Cleanup completed. Kept the latest 10 versions."
else
    echo "Function has $TOTAL_VERSIONS versions or fewer. No cleanup needed."
fi
