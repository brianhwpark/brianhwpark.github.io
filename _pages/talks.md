---
layout: archive
title: "Conferences"
permalink: /talks/
author_profile: true

#<sup>*</sup> Equal authorship  ADD AT END IF EQUAL AUTHORSHIP
---

{% include base_path %}

{% for post in site.talks reversed %}
  {% include archive-single.html %}
{% endfor %}
