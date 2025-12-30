graph LR
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Graph
    direction LR
    x660da1d01e230321(["workflow_graph"]):::dispatched --> xc11069275cfeb620(["readme"]):::queued
    x83c90c487d16eadc(["schedule_file"]):::queued --> xd1e486155305a9d8(["schedule_ical_data"]):::queued
    xd1e486155305a9d8(["schedule_ical_data"]):::queued --> xe96618267648362b(["schedule_ical_file"]):::queued
    x83c90c487d16eadc(["schedule_file"]):::queued --> x7f26ad8951796691(["schedule_page_data"]):::queued
    
  end
