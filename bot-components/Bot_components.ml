(** Wrapper module that re-exports all bot-components modules for backward compatibility.
    This allows code to use [open Bot_components] and access modules like [Bot_components.Utils]. *)

(** Core modules *)
module Bot_info = Bot_info

(** Utility modules *)
module Utils = Utils

module String_utils = String_utils
module HTTP_utils = HTTP_utils
module Git_utils = Git_utils
module Minimize_parser = Minimize_parser

(** CI utilities *)
module CI_utils = Pipeline

(** GraphQL infrastructure *)
module GraphQL_query = GraphQL_query

(** GitHub modules *)
module GitHub_app = GitHub_app

module GitHub_automation = GitHub_automation
module GitHub_GitLab_sync = GitHub_GitLab_sync
module GitHub_GraphQL = GitHub_GraphQL
module GitHub_ID = GitHub_ID
module Github_installations = GitHub_installations
module GitHub_mutations = GitHub_mutations
module GitHub_queries = GitHub_queries
module GitHub_subscriptions = GitHub_subscriptions
module GitHub_types = GitHub_types

(** GitLab modules *)
module GitLab_GraphQL = GitLab_GraphQL

module GitLab_mutations = GitLab_mutations
module GitLab_queries = GitLab_queries
module GitLab_subscriptions = GitLab_subscriptions
module GitLab_types = GitLab_types
