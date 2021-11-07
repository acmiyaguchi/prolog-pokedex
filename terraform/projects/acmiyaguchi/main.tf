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

resource "google_container_registry" "registry" {
  location = "US"
}

resource "google_cloudbuild_trigger" "deploy-cloud-run" {
  github {
    name  = "prolog-pokemon"
    owner = "acmiyaguchi"
    push {
      branch       = "^deploy$"
      invert_regex = false
    }
  }
  substitutions = {
    _REGION = local.region
  }
  filename = "cloudbuild.yaml"
}

// A useful blog post: https://ruanmartinelli.com/posts/terraform-cloud-run
resource "google_project_service" "run" {
  service = "run.googleapis.com"
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