from fastapi.testclient import TestClient

from app.main import app


def test_healthz():
    response = TestClient(app).get("/healthz")
    assert response.status_code == 200
    assert response.json()["status"] == "ok"


def test_openapi_has_core_routes():
    paths = app.openapi()["paths"]
    assert "/api/datasets/upload" in paths
    assert "/api/clustering/{dataset_id}/unsupervised" in paths
    assert "/api/batch-difference/{dataset_id}/run" in paths
    assert "/api/batch-difference/{dataset_id}/validate" in paths
    assert "/api/batch-difference/jobs/{job_id}/point/{point_id}" in paths
