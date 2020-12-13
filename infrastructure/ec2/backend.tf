terraform {
  backend "s3" {
    bucket  = "m-terraform-state"
    key     = "cs598/ec2.tfstate"
    region  = "us-east-1"
    encrypt = true
  }
}