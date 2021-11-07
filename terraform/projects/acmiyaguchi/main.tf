terraform {
  backend "gcs" {
    bucket = "acmiyaguchi-terraform"
    prefix = "prolog-pokemon"
  }
}

locals {
  project_id = "acmiyaguchi"
  region     = "us-central1"
}

provider "google" {
  project = local.project_id
  region  = local.region
}

data "google_project" "project" {}

resource "google_container_registry" "registry" {
  location = "US"
}

// https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/cloudbuild_trigger
// https://cloud.google.com/build/docs/configuring-builds/substitute-variable-values
// https://cloud.google.com/build/docs/cloud-build-service-account
// https://cloud.google.com/build/docs/securing-builds/store-manage-build-logs
resource "google_cloudbuild_trigger" "deploy-cloud-run" {
  github {
    name  = "prolog-pokemon"
    owner = "acmiyaguchi"
    push {
      branch       = "^main$"
      invert_regex = false
    }
  }
  substitutions = {
    _REGION = local.region
  }
  filename = "cloudbuild.yaml"
  service_account = google_service_account.cloudbuild.id
  depends_on = [
    google_project_iam_member.act-as,
    google_project_iam_member.logs-writer
  ]
}

resource "google_service_account" "cloudbuild" {
  account_id = "cloudbuild-prolog-pokedex"
}

resource "google_project_iam_member" "act-as" {
  project = data.google_project.project.project_id
  role    = "roles/iam.serviceAccountUser"
  member  = "serviceAccount:${google_service_account.cloudbuild.email}"
}

resource "google_project_iam_member" "logs-writer" {
  project = data.google_project.project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.cloudbuild.email}"
}

resource "google_project_iam_member" "run-admin" {
  project = data.google_project.project.project_id
  role   = "roles/run.admin"
  member  = "serviceAccount:${google_service_account.cloudbuild.email}"
}

resource "google_project_iam_member" "storage-admin" {
  project = data.google_project.project.project_id
  role    = "roles/storage.admin"
  member = "serviceAccount:${google_service_account.cloudbuild.email}"
}

// A useful blog post: https://ruanmartinelli.com/posts/terraform-cloud-run
resource "google_project_service" "run" {
  service = "run.googleapis.com"
}

// necessary for manual triggering of builds, at the very least
resource "google_project_service" "iam" {
  service = "iam.googleapis.com"
}

resource "google_cloud_run_service" "default" {
  depends_on = [google_project_service.run]

  name     = "prolog-pokedex"
  location = local.region

  template {
    spec {
      containers {
        image = "gcr.io/${local.project_id}/prolog-pokedex"
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}

resource "google_cloud_run_service_iam_member" "all-users" {
  service  = google_cloud_run_service.default.name
  location = google_cloud_run_service.default.location
  role     = "roles/run.invoker"
  member   = "allUsers"
}

output "service_url" {
  value = google_cloud_run_service.default.status[0].url
}