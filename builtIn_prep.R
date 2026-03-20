library(tidyverse)
library(rvest)
library(polite)


# Create Scrapping function for scrapping of job title and job description later

get1 <- function(html, css) {
  node <- html_element(html, css)
  if (inherits(node, "xml_missing") || is.na(node)) return("")
  html_text2(node)
}

first_nonempty <- function(html, selectors) {
  for (sel in selectors) {
    txt <- get1(html, sel)
    if (nzchar(txt)) return(txt)
  }
  ""
}

scrape_builtin <- function(url, ua = "607-Project-3/edu (rvest+polite)") {
  ses <- polite::bow(url, user_agent = ua, force = TRUE)
  pg  <- polite::scrape(ses)
  
  # Title
  title <- first_nonempty(pg, c("h1", "meta[property='og:title']"))
  if (!nzchar(title)) title <- NA_character_
  
  # Description: try a few common containers, fall back to main/body
  desc <- first_nonempty(pg, c(
    "article",
    "[data-testid='job-description']",
    "[data-qa='job-description']",
    "div[class*='job-description']",
    "section[class*='description']"
  ))
  if (!nzchar(desc)) desc <- get1(pg, "[role='main']")
  if (!nzchar(desc)) desc <- get1(pg, "body")
  if (!nzchar(desc)) desc <- NA_character_
  
  tibble(
    job_title       = title,
    job_description = desc,
    job_url         = url
  )
}

# Organize all job posting URLs into a tibble

builtin_urls <- tibble(
  job_url = c(
    "https://builtin.com/job/manager-data-scientist-alternate-data-strategy/8162509",
    "https://builtin.com/job/principal-associate-data-scientist-ai-software-engineering/8017518",
    "https://builtin.com/job/principal-associate-data-scientist-us-card-new-credit-team/8029691",
    "https://builtin.com/job/senior-associate-data-scientist/8432791",
    "https://builtin.com/job/data-scientist-ii-real-world-evidence-rwe-pharma-r-d/8031762",
    "https://builtin.com/job/staff-data-scientist-growth-care/8019471",
    "https://www.builtinnyc.com/job/senior-staff-data-scientist-invest/8697390",
    "https://www.builtinnyc.com/job/staff-data-scientist-borrow/8290699",
    "https://www.builtinnyc.com/job/field-data-scientist-ai-deployment/7405417",
    "https://www.builtinnyc.com/job/ai-genai-data-scientist-senior-manager/8258875",
    "https://www.builtinnyc.com/job/data-scientist/8446518"
  ))


# Scrape --> builtin_raw 
safe_scrape <- purrr::safely(scrape_builtin)

builtin_raw <- builtin_urls |> 
  mutate(res = map(job_url, safe_scrape)) |> 
  transmute(
    job_url,
    job_title  = map_chr(res, ~ .x$result$job_title %||% NA_character_),
    job_description = map_chr(res, ~ .x$result$job_description %||% NA_character_))


# Double checking that job description returns with page body text for skill, soft skill keywords, and frequency comparison later - text number appear = good 

nchar(builtin_raw$job_description)


# Scrape and  Manually add company and industry from data seen on job posting website
builtin_jobs <- builtin_raw |> 
  mutate(
    company = case_when(
      str_detect(job_url, "capital|cap1") ~ "Capital One",
      str_detect(job_url, "nylife|new-york-life|senior-associate-data-scientist/8432791")
      ~ "NY Life",
      str_detect(job_url, "tempus") ~ "Tempus AI",
      str_detect(job_url, "grow") ~ "Grow Therapy",
      str_detect(job_url, "sofi") ~ "SoFi",
      str_detect(job_url, "braze") ~ "Braze",
      str_detect(job_url, "prc") ~ "PRC",
      str_detect(job_url, "octus") ~ "Octus",
      TRUE ~ NA_character_
    ),
    industry = case_when(
      company %in% c("Capital One", "SoFi", "NY Life") ~ "Finance",
      company == "Tempus AI" ~ "Healthcare / Pharma",
      company == "Grow Therapy" ~ "Healthcare",
      company == "Braze" ~ "SaaS / Martech",
      company %in% c("PRC", "Octus") ~ "Consulting",
      TRUE ~ NA_character_
    ),
    source = "builtin",
    job_id = row_number()
  ) %>%
  select(job_id, job_title, company, industry, source, job_url, job_description)


# extract skills seen on job descriptions
skill_keywords <- c(
  # languages
  "python","r","sql","scala","java",
  # libraries / ml
  "pytorch","tensorflow","scikit","sklearn","xgboost",
  # platforms
  "aws","azure","gcp","databricks","snowflake","spark",
  # bi / viz
  "tableau","power bi")


builtin_job_skills <- builtin_jobs %>%
  transmute(job_id, text = tolower(job_description %||% "")) %>%
  filter(nzchar(text)) %>%
  crossing(skill = skill_keywords) %>%
  mutate(rx = paste0("\\b", skill, "\\b")) %>%
  filter(str_detect(text, regex(rx, ignore_case = TRUE))) %>%
  distinct(job_id, skill)
