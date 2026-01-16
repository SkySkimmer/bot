module GetRetriedJobs =
  [%graphql
  {|
  query getRetriedJobs($fullPath: ID!, $jobId: JobID!) {
    project(fullPath: $fullPath) {
      job(id: $jobId) {
        pipeline {
          ... on Pipeline {
            jobs(retried: true, first: 100) {
              count
              nodes {
                name
              }
            }
          }
        }
      }
    }
  }
|}]
