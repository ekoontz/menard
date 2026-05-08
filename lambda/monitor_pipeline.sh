#!/bin/bash

# Script to monitor CodePipeline execution
# Usage: ./tail-pipeline.sh <pipeline-name> [region]

PIPELINE_NAME="$1"
REGION="${2:-us-east-1}"
POLL_INTERVAL=10  # seconds

if [ -z "$PIPELINE_NAME" ]; then
    echo "Usage: $0 <pipeline-name> [region]"
    exit 1
fi

echo "Monitoring pipeline: $PIPELINE_NAME in region: $REGION"
echo "Press Ctrl+C to stop monitoring"
echo "================================"

# Get the latest execution ID
get_latest_execution() {
    aws codepipeline list-pipeline-executions \
        --pipeline-name "$PIPELINE_NAME" \
        --region "$REGION" \
        --max-items 1 \
        --query 'pipelineExecutionSummaries[0].pipelineExecutionId' \
        --output text 2>/dev/null
}

# Check if pipeline exists
check_pipeline_exists() {
    aws codepipeline get-pipeline \
        --name "$PIPELINE_NAME" \
        --region "$REGION" \
        >/dev/null 2>&1
    return $?
}

# Monitor execution status using only AWS CLI
monitor_execution() {
    local execution_id="$1"
    local last_status=""
    
    echo "Monitoring execution: $execution_id"
    echo ""
    
    while true; do
        # Get current execution status
        current_status=$(aws codepipeline get-pipeline-execution \
            --pipeline-name "$PIPELINE_NAME" \
            --pipeline-execution-id "$execution_id" \
            --region "$REGION" \
            --query 'pipelineExecution.status' \
            --output text 2>/dev/null)
        
        if [ $? -ne 0 ] || [ -z "$current_status" ] || [ "$current_status" = "None" ]; then
            echo "Error: Could not get execution details for ID: $execution_id"
            echo "This might mean:"
            echo "  - The execution ID is invalid"
            echo "  - The execution has been deleted"
            echo "  - You don't have permissions"
            echo ""
            echo "Trying to get the latest execution instead..."
            
            # Try to get a new execution ID
            new_execution_id=$(get_latest_execution)
            if [ "$new_execution_id" != "None" ] && [ ! -z "$new_execution_id" ] && [ "$new_execution_id" != "$execution_id" ]; then
                echo "Found newer execution: $new_execution_id"
                execution_id="$new_execution_id"
                continue
            else
                echo "No valid executions found. Exiting."
                break
            fi
        fi
        
        # Print status change
        if [ "$current_status" != "$last_status" ]; then
            timestamp=$(date '+%Y-%m-%d %H:%M:%S')
            echo "[$timestamp] Pipeline Status: $current_status"
            last_status="$current_status"
        fi
        
        # Get stage statuses
        echo "Stage Details:"
        aws codepipeline get-pipeline-state \
            --name "$PIPELINE_NAME" \
            --region "$REGION" \
            --query 'stageStates[].[stageName,latestExecution.status]' \
            --output table 2>/dev/null || echo "  Could not retrieve stage details"
        
        # Check if pipeline is finished
        if [[ "$current_status" == "Succeeded" || "$current_status" == "Failed" || "$current_status" == "Stopped" ]]; then
            echo ""
            echo "Pipeline execution completed with status: $current_status"
            
            # Show final execution details
            echo ""
            echo "Final Execution Summary:"
            aws codepipeline get-pipeline-execution \
                --pipeline-name "$PIPELINE_NAME" \
                --pipeline-execution-id "$execution_id" \
                --region "$REGION" \
                --output table 2>/dev/null
            break
        fi
        
        echo "---"
        sleep $POLL_INTERVAL
    done
}

# Main execution
echo "Checking if pipeline exists..."
if ! check_pipeline_exists; then
    echo "Error: Pipeline '$PIPELINE_NAME' not found in region '$REGION'"
    echo "Available pipelines:"
    aws codepipeline list-pipelines --region "$REGION" --query 'pipelines[].name' --output table
    exit 1
fi

echo "Pipeline found. Getting latest execution..."
EXECUTION_ID=$(get_latest_execution)

if [ "$EXECUTION_ID" = "None" ] || [ -z "$EXECUTION_ID" ]; then
    echo "No executions found for pipeline: $PIPELINE_NAME"
    echo "You can start a new execution with:"
    echo "aws codepipeline start-pipeline-execution --name $PIPELINE_NAME --region $REGION"
else
    monitor_execution "$EXECUTION_ID"
fi
