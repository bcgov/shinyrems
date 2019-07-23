# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 4. Test my package

devtools::test()
rhub::check_for_cran()

# 5. Deployment elements

## 5.1 If you want to deploy on RStudio related platforms
golem::add_rconnect_file()

## 5.2 If you want to deploy via a Dockerfile
golem::add_dockerfile()
