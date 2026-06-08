# ============================================================
# ui.R — Routing par rôle (ONG / Admin)
# ============================================================

source("modules/auth.R")

common_css <- tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap');

  /* ══ VARIABLES ══════════════════════════════════════════ */
  body, body.theme-dark {
    --c-bg:       #1b1f2e;
    --c-surface:  #232739;
    --c-surface2: #2a2f42;
    --c-border:   #333750;
    --c-text:     #cdd2e0;
    --c-muted:    #6e778f;
    --c-faint:    #3a3f55;
    --c-accent:   #e8a020;
    --c-boxhead:  #003270;
    --c-accent2:  #c86010;
    --c-glow:     rgba(232,160,32,.13);
    --c-danger:   #e06060;
    --c-success:  #40c070;
    --c-badge:    #1b1f2e;
  }
  body.theme-light {
    --c-bg:       #eef0f5;
    --c-surface:  #ffffff;
    --c-surface2: #f4f5fa;
    --c-border:   #d8dce8;
    --c-text:     #1e2235;
    --c-muted:    #5a627a;
    --c-faint:    #b0b8cc;
    --c-accent:   #b06010;
    --c-boxhead:  #003270;
    --c-accent2:  #8a4a0a;
    --c-glow:     rgba(176,96,16,.12);
    --c-danger:   #cc3333;
    --c-success:  #1a8040;
    --c-badge:    #ffffff;
  }

  /* ══ BASE ════════════════════════════════════════════════ */
  *, *::before, *::after { box-sizing:border-box !important; }
  html, body { font-family:'Inter',sans-serif !important; font-size:13px !important; }
  body, .content-wrapper, .main-footer, .wrapper {
    background:var(--c-bg) !important;
    color:var(--c-text) !important;
  }
  .content { padding:12px 14px !important; }

  /* ══ HEADER ══════════════════════════════════════════════ */
  .skin-black .main-header .navbar,
  .skin-black .main-header .logo {
    background:#141828 !important;
    border-bottom:1px solid #1e2438 !important;
    height:40px !important; min-height:40px !important;
  }
  .skin-black .main-header .logo {
    line-height:40px !important;
    font-size:13px !important; font-weight:600 !important;
    color:#c8d0e0 !important;
    border-right:1px solid #1e2438 !important;
    letter-spacing:.2px;
    font-family:'Inter',sans-serif !important;
  }
  .skin-black .main-header .navbar .sidebar-toggle {
    height:40px !important; padding:0 14px !important;
    color:#6e778f !important; font-size:14px !important;
    display:flex !important; align-items:center !important;
  }
  .skin-black .main-header .navbar .sidebar-toggle:hover {
    background:rgba(232,160,32,.08) !important; color:#e8a020 !important;
  }
  .skin-black .main-header .navbar > .nav > li > a {
    height:40px !important; line-height:40px !important;
    padding:0 10px !important; color:#6e778f !important;
    font-size:12px !important; font-family:'Inter',sans-serif !important;
  }
  .skin-black .main-header .navbar > .nav > li > a:hover {
    background:rgba(232,160,32,.08) !important; color:#e8a020 !important;
  }

  /* ══ SIDEBAR ═════════════════════════════════════════════ */
  .skin-black .main-sidebar,
  .skin-black .left-side {
    background:#141828 !important;
    border-right:1px solid #1e2438 !important;
    padding-top:40px !important;
  }
  .skin-black .sidebar { padding-bottom:72px !important; }
  .skin-black .sidebar-menu > li > a {
    color:#6e778f !important; font-size:11.5px !important;
    font-family:'Inter',sans-serif !important; font-weight:500 !important;
    border-left:3px solid transparent !important;
    padding:8px 12px !important; min-height:36px !important;
    display:flex !important; align-items:center !important;
    transition:all .15s !important;
  }
  .skin-black .sidebar-menu > li.active > a,
  .skin-black .sidebar-menu > li > a:hover {
    color:#e8a020 !important;
    background:rgba(232,160,32,.07) !important;
    border-left-color:#e8a020 !important;
  }
  .skin-black .sidebar-menu > li > a > .fa {
    color:#e8a020 !important; width:15px !important;
    font-size:12px !important; margin-right:8px !important;
    text-align:center; opacity:.85;
  }
  .sidebar-menu .header {
    color:#3a3f55 !important; font-size:9px !important;
    letter-spacing:1.5px; text-transform:uppercase;
    padding:10px 12px 3px !important; font-family:'Inter',sans-serif !important;
  }
  .skin-black .sidebar a { color:#6e778f !important; }


   /* ══ BOX ═════════════════════════════════════════════════ */
  .box {
  border-left:none !important;
  border-radius:0px !important;
  border:1px solid var(--c-border) !important;
  box-shadow:none !important;
}
.box-header {
  background:var(--c-boxhead) !important;  /* header coloré comme soilprofile */
  padding:4px 9px !important;
}
.box-title { color:#fff !important; font-size:10.5px !important; }

  /* ══ BOX ═════════════════════════════════════════════════ 
  .box {
    background:var(--c-surface) !important;
    border:1px solid var(--c-border) !important;
    border-radius:6px !important;
    box-shadow:0 1px 4px rgba(0,0,0,.25) !important;
    margin-bottom:10px !important;
    overflow:visible !important;
  }
  */
  
  /* Accent discret : fine bande gauche au lieu de top gradient 
  .box { border-left:3px solid var(--c-accent) !important; }
  .box-header {
    background:var(--c-surface2) !important;
    border-bottom:1px solid var(--c-border) !important;
    padding:1px 12px !important;
    min-height:36px !important;
    display:flex !important; align-items:center !important;
    border-radius:5px 5px 0 0 !important;
  }
  .box-header.with-border { border-bottom:1px solid var(--c-border) !important; }
  .box-title {
    font-family:'Inter',sans-serif !important;
    font-size:12px !important; font-weight:600 !important;
    color:var(--c-text) !important; letter-spacing:.1px !important;
    display:flex !important; align-items:center !important; gap:7px !important;
  }
  */
  
  .box-badge {
    background:var(--c-accent) !important;
    color:var(--c-badge) !important; border-radius:3px !important;
    width:18px !important; height:18px !important;
    display:inline-flex !important; align-items:center !important;
    justify-content:center !important; font-size:9px !important;
    font-weight:700 !important; flex-shrink:0 !important;
    font-family:'Inter',sans-serif !important;
  }
  .box-title .fa {
    color:var(--c-accent) !important; font-size:11px !important; opacity:.8 !important;
  }
  .box-header .btn,
  .box-header .btn-box-tool {
    color:var(--c-faint) !important; background:transparent !important;
    border:none !important; padding:2px 5px !important;
    font-size:13px !important; line-height:1 !important;
    transition:color .12s !important;
  }
  .box-header .btn:hover,
  .box-header .btn-box-tool:hover {
    color:var(--c-accent) !important; background:transparent !important;
  }
  .box-body {
    background:var(--c-surface) !important;
    padding:12px 14px !important;
    border-radius:0 0 5px 5px !important;
  }

  /* ══ LABELS & INPUTS ═════════════════════════════════════ */
  label, .control-label {
  font-size:8px         /* taille — essaie 11px, 12px */
  font-weight:400          /* épaisseur — 400=normal, 600=semibold */
  color:var(--c-muted)     /* couleur */
  text-transform:none      /* none / uppercase */
  letter-spacing:.1px      /* espacement lettres */
  margin-bottom:3px        /* espace entre label et champ */
}
  .required-star { color:var(--c-danger) !important; }
  .form-group { margin-bottom:8px !important; }

  input[type='text'],input[type='number'],input[type='email'],
  input[type='password'],input[type='date'],textarea,.form-control {
    background:var(--c-surface2) !important;
    color:var(--c-text) !important;
    border:1px solid var(--c-border) !important;
    border-radius:4px !important;
    font-size:8px !important; font-family:'Inter',sans-serif !important;
    padding:2px 8px !important;
    height:20px !important; min-height:20px !important; max-height:20px !important;
    line-height:1.4 !important; width:100% !important;
    transition:border-color .12s, box-shadow .12s !important;
    box-shadow:none !important;
  }
  textarea, textarea.form-control {
    height:auto !important; min-height:56px !important; max-height:none !important;
    padding:5px 8px !important; resize:vertical !important;
  }
  input:focus, textarea:focus, .form-control:focus {
    border-color:var(--c-accent) !important;
    box-shadow:0 0 0 2px var(--c-glow) !important;
    outline:none !important;
  }
  input::placeholder,textarea::placeholder,.form-control::placeholder {
    color:var(--c-faint) !important; font-size:11px !important;
  }

  /* Selectize compact */
  .selectize-input,.selectize-control.single .selectize-input {
    background:var(--c-surface2) !important; color:var(--c-text) !important;
    border:1px solid var(--c-border) !important; border-radius:4px !important;
    min-height:28px !important; height:28px !important;
    padding:3px 28px 3px 8px !important;
    box-shadow:none !important; font-size:12px !important;
    font-family:'Inter',sans-serif !important; display:flex !important;
    align-items:center !important; line-height:1 !important;
    transition:border-color .12s !important;
  }
  .selectize-input.focus { border-color:var(--c-accent) !important; box-shadow:0 0 0 2px var(--c-glow) !important; }
  .selectize-input > input { font-size:12px !important; margin:0 !important; padding:0 !important; height:auto !important; min-height:auto !important; }
  .selectize-control.single .selectize-input:after {
    right:8px !important; top:50% !important; margin-top:-3px !important;
    border-color:var(--c-muted) transparent transparent !important;
    border-width:5px 4px 0 !important;
  }
  .selectize-dropdown {
    background:var(--c-surface) !important; color:var(--c-text) !important;
    border:1px solid var(--c-border) !important; border-radius:4px !important;
    box-shadow:0 6px 20px rgba(0,0,0,.35) !important;
    font-size:12px !important; font-family:'Inter',sans-serif !important;
    margin-top:2px !important;
  }
  .selectize-dropdown .option { padding:6px 8px !important; line-height:1.3; }
  .selectize-dropdown .active { background:rgba(232,160,32,.1) !important; color:var(--c-accent) !important; }

  /* Date & input-group */
  .input-group .form-control { border-radius:4px 0 0 4px !important; }
  .input-group-addon {
    background:var(--c-surface2) !important; color:var(--c-muted) !important;
    border:1px solid var(--c-border) !important; border-radius:0 4px 4px 0 !important;
    padding:0 8px !important; font-size:11px !important;
    height:28px !important; line-height:26px !important;
  }

  /* Checkbox */
  input[type='checkbox'] { accent-color:var(--c-accent) !important; width:13px !important; height:13px !important; max-height:13px !important; }
  .checkbox label,.shiny-input-checkboxgroup label {
    color:var(--c-text) !important; font-size:11.5px !important;
    text-transform:none !important; letter-spacing:0 !important; font-weight:400 !important;
  }

  /* ══ BOUTONS ═════════════════════════════════════════════ */
  .btn,.action-button {
    font-family:'Inter',sans-serif !important;
    font-size:11.5px !important; font-weight:500 !important;
    border-radius:4px !important;
    padding:4px 12px !important; min-height:28px !important;
    height:28px !important; line-height:1.2 !important;
    transition:all .12s !important; cursor:pointer !important;
    display:inline-flex !important; align-items:center !important; gap:5px !important;
  }
  .btn-default,.btn-default.action-button {
    background:var(--c-surface2) !important; color:var(--c-text) !important;
    border:1px solid var(--c-border) !important;
  }
  .btn-default:hover { background:var(--c-border) !important; }
  .btn-primary { background:var(--c-accent) !important; color:#1b1f2e !important; border:none !important; font-weight:600 !important; }
  .btn-primary:hover { filter:brightness(1.1) !important; }

  .btn-del {
    background:rgba(224,96,96,.12) !important; color:var(--c-danger) !important;
    border:1px solid rgba(224,96,96,.2) !important;
    padding:2px 6px !important; height:22px !important; min-height:22px !important;
    font-size:10px !important; border-radius:3px !important;
  }
  .btn-del:hover { background:rgba(224,96,96,.2) !important; }

  .btn-add-line {
    background:transparent !important; color:var(--c-accent) !important;
    border:1px dashed rgba(232,160,32,.35) !important; border-radius:4px !important;
    padding:6px !important; width:100% !important; font-size:11.5px !important;
    height:30px !important; min-height:30px !important;
    opacity:.7 !important; transition:all .15s !important; margin-top:5px !important;
    display:flex !important; align-items:center !important;
    justify-content:center !important; gap:5px !important; font-weight:500 !important;
  }
  .btn-add-line:hover { opacity:1 !important; background:rgba(232,160,32,.07) !important; border-color:var(--c-accent) !important; }

  .btn-submit {
    background:var(--c-accent) !important; color:#1b1f2e !important;
    border:none !important; border-radius:4px !important;
    font-weight:700 !important; font-size:12px !important;
    padding:0 18px !important; height:30px !important; min-height:30px !important;
    letter-spacing:.3px !important; transition:filter .12s !important;
    display:inline-flex !important; align-items:center !important; gap:5px !important;
  }
  .btn-submit:hover { filter:brightness(1.1) !important; }
  .btn-reset {
    background:transparent !important; color:var(--c-muted) !important;
    border:1px solid var(--c-border) !important; border-radius:4px !important;
    padding:0 12px !important; height:30px !important; min-height:30px !important;
    font-size:11.5px !important; margin-right:6px !important;
    display:inline-flex !important; align-items:center !important;
  }
  .btn-reset:hover { color:var(--c-text) !important; border-color:var(--c-muted) !important; }

  /* ══ SECTIONS INTERNES ═══════════════════════════════════ */
  .inner-section { margin-top:10px !important; padding-top:10px !important; border-top:1px solid var(--c-border) !important; }
  .inner-title {
    color:var(--c-accent) !important; font-size:9.5px !important; font-weight:600 !important;
    text-transform:uppercase !important; letter-spacing:1px !important;
    margin-bottom:7px !important; opacity:.8; font-family:'Inter',sans-serif !important;
    text-transform:none !important;
  }
  .hint-text { color:var(--c-faint) !important; font-size:10px !important; margin-top:2px !important; }

  /* ══ ROW BLOCK ═══════════════════════════════════════════ */
  .row-block {
    background:var(--c-surface2) !important;
    border:1px solid var(--c-border) !important;
    border-radius:5px !important; padding:10px 12px !important;
    margin-bottom:7px !important;
  }
  .row-block:hover { border-color:rgba(232,160,32,.3) !important; }
  .row-block strong { color:var(--c-text) !important; font-size:11.5px !important; font-weight:600 !important; }

  /* ══ SUBMIT BAR ══════════════════════════════════════════ */
  .submit-bar {
    background:var(--c-surface) !important; border:1px solid var(--c-border) !important;
    border-radius:6px !important; padding:10px 14px !important;
    display:flex !important; align-items:center !important;
    justify-content:space-between !important; gap:10px !important;
    margin:10px 0 18px !important;
  }
  .submit-info { color:var(--c-muted) !important; font-size:11px !important; line-height:1.5 !important; }
  .submit-info strong { color:var(--c-text) !important; }

  /* ══ PAGE HEADER ═════════════════════════════════════════ */
  .page-header { margin-bottom:12px !important; padding-bottom:10px !important; border-bottom:1px solid var(--c-border) !important; }
  .page-title { font-family:'Inter',sans-serif !important; font-size:15px !important; font-weight:700 !important; color:var(--c-text) !important; }
  .page-subtitle { color:var(--c-muted) !important; font-size:10.5px !important; margin-top:2px !important; }

  /* ══ BADGES ══════════════════════════════════════════════ */
  .statut-badge { font-size:9px !important; padding:1px 7px !important; border-radius:3px !important; font-weight:600 !important; letter-spacing:.3px !important; }
  .statut-vide    { background:var(--c-border) !important; color:var(--c-faint) !important; }
  .statut-partiel { background:rgba(232,160,32,.12) !important; color:var(--c-accent) !important; border:1px solid rgba(232,160,32,.25) !important; }
  .statut-complet { background:rgba(64,192,112,.12) !important; color:var(--c-success) !important; border:1px solid rgba(64,192,112,.25) !important; }
  .notif-badge { background:var(--c-danger) !important; color:#fff !important; border-radius:3px !important; padding:1px 5px !important; font-size:9px !important; font-weight:700 !important; margin-left:4px !important; vertical-align:middle !important; }

  /* ══ TOGGLE ══════════════════════════════════════════════ */
  .toggle-track { width:34px !important; height:18px !important; border-radius:9px !important; background:var(--c-border) !important; position:relative !important; cursor:pointer !important; transition:background .2s !important; display:inline-block !important; vertical-align:middle !important; }
  body.theme-light .toggle-track { background:var(--c-accent) !important; }
  .toggle-thumb { width:14px !important; height:14px !important; border-radius:50% !important; background:#fff !important; position:absolute !important; top:2px !important; left:2px !important; transition:transform .2s !important; box-shadow:0 1px 3px rgba(0,0,0,.3) !important; }
  body.theme-light .toggle-thumb { transform:translateX(16px) !important; }

  /* ══ DATATABLES ══════════════════════════════════════════ */
  .dataTables_wrapper { color:var(--c-text) !important; font-family:'Inter',sans-serif !important; font-size:12px !important; }
  table.dataTable { color:var(--c-text) !important; border-collapse:collapse !important; }
  table.dataTable thead th { background:var(--c-surface2) !important; color:var(--c-muted) !important; border-bottom:1px solid var(--c-border) !important; font-size:10px !important; text-transform:uppercase !important; letter-spacing:.6px !important; padding:7px 10px !important; font-weight:600 !important; }
  table.dataTable tbody tr { background:var(--c-surface) !important; }
  table.dataTable tbody tr:nth-child(even) { background:var(--c-surface2) !important; }
  table.dataTable tbody td { border-bottom:1px solid var(--c-border) !important; font-size:11.5px !important; padding:6px 10px !important; color:var(--c-text) !important; }
  table.dataTable tbody tr:hover td { background:rgba(232,160,32,.07) !important; }
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper .dataTables_length select { background:var(--c-surface2) !important; color:var(--c-text) !important; border:1px solid var(--c-border) !important; border-radius:4px !important; padding:3px 7px !important; font-size:11px !important; height:26px !important; }
  .dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_filter,.dataTables_wrapper .dataTables_length,.dataTables_wrapper .dataTables_paginate { color:var(--c-muted) !important; font-size:10.5px !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button { color:var(--c-muted) !important; border-radius:3px !important; padding:3px 8px !important; font-size:10.5px !important; border:1px solid transparent !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current { background:rgba(232,160,32,.12) !important; color:var(--c-accent) !important; border-color:rgba(232,160,32,.25) !important; }

  /* Suivi enquete */
  .tracking-meta { display:grid; grid-template-columns:repeat(auto-fit, minmax(150px, 1fr)); gap:10px; align-items:stretch; }
  .tracking-meta-selector { grid-column:span 2; min-width:220px; }
  .tracking-meta-item { background:var(--c-surface2); border:1px solid var(--c-border); border-radius:6px; padding:8px 10px; min-height:54px; overflow:hidden; }
  .tracking-meta-label { color:var(--c-muted); font-size:9px; text-transform:uppercase; letter-spacing:.7px; margin-bottom:4px; }
  .tracking-meta-value { color:var(--c-text); font-size:13px; font-weight:700; overflow-wrap:anywhere; }
  .tracking-meta-value.is-danger { color:var(--c-danger); }
  .tracking-meta-value.is-success { color:var(--c-success); }
  .kpi-grid { display:grid; grid-template-columns:repeat(4, minmax(130px, 1fr)); gap:10px; margin-bottom:10px; }
  .kpi-card { background:var(--c-surface); border:1px solid var(--c-border); border-radius:6px; padding:12px; min-height:112px; }
  .kpi-label { color:var(--c-muted); font-size:10px; font-weight:600; margin-bottom:6px; }
  .kpi-value { color:var(--c-text); font-size:24px; font-weight:800; line-height:1; }
  .kpi-sub { color:var(--c-muted); font-size:10px; margin-top:4px; }
  .mini-progress { height:6px; border-radius:999px; overflow:hidden; background:var(--c-border); margin-top:12px; }
  .mini-progress-fill { height:100%; border-radius:999px; }
  .fill-green { background:#35b86b; }
  .fill-orange { background:#e8a020; }
  .fill-red { background:#df5b5b; }
  .fill-blue { background:#4b8ee8; }
  .status-pill { display:inline-flex; align-items:center; justify-content:center; min-width:70px; padding:2px 8px; border-radius:999px; font-size:10px; font-weight:700; }
  .status-submitted { background:rgba(64,192,112,.14); color:var(--c-success); border:1px solid rgba(64,192,112,.25); }
  .status-draft { background:rgba(232,160,32,.14); color:var(--c-accent); border:1px solid rgba(232,160,32,.25); }
  .status-absent { background:rgba(224,96,96,.14); color:var(--c-danger); border:1px solid rgba(224,96,96,.25); }
  .status-neutral { background:var(--c-surface2); color:var(--c-muted); border:1px solid var(--c-border); }
  .alert-list { display:flex; flex-direction:column; gap:7px; }
  .tracking-alert { border:1px solid var(--c-border); background:var(--c-surface2); border-radius:6px; padding:9px 10px; font-size:11px; color:var(--c-text); }
  .tracking-alert.warning { border-left:3px solid var(--c-accent); }
  .tracking-alert.danger { border-left:3px solid var(--c-danger); }
  .tracking-alert.success { border-left:3px solid var(--c-success); }
  .activity-feed { display:flex; flex-direction:column; gap:7px; }
  .activity-item { display:grid; grid-template-columns:46px minmax(90px, 1fr); gap:8px; align-items:start; padding:8px 0; border-bottom:1px solid var(--c-border); }
  .activity-time { color:var(--c-muted); font-size:10px; }
  .activity-title { color:var(--c-text); font-size:11px; font-weight:700; }
  .activity-text { color:var(--c-muted); font-size:10.5px; margin-top:2px; }
  .tracking-actions { display:flex; gap:8px; flex-wrap:wrap; align-items:center; }
  .table-action { border:1px solid var(--c-border); background:var(--c-surface2); color:var(--c-text); border-radius:4px; padding:3px 8px; font-size:10px; margin-right:4px; }
  .table-action:hover { color:var(--c-accent); border-color:rgba(232,160,32,.35); }
  @media (max-width:900px) {
    .tracking-meta { grid-template-columns:repeat(2, minmax(0, 1fr)); }
    .tracking-meta-selector { grid-column:1 / -1; min-width:0; }
  }
  @media (max-width:520px) {
    .tracking-meta { grid-template-columns:1fr; }
    .tracking-meta-selector { grid-column:1; }
  }

  /* ══ MODALS ══════════════════════════════════════════════ */
  .modal-content { background:var(--c-surface) !important; color:var(--c-text) !important; border:1px solid var(--c-border) !important; border-radius:6px !important; box-shadow:0 16px 48px rgba(0,0,0,.5) !important; }
  .modal-header { background:var(--c-surface2) !important; border-bottom:1px solid var(--c-border) !important; padding:10px 14px !important; border-radius:5px 5px 0 0 !important; }
  .modal-title { font-size:13px !important; font-weight:600 !important; color:var(--c-text) !important; font-family:'Inter',sans-serif !important; }
  .modal-footer { border-top:1px solid var(--c-border) !important; padding:8px 14px !important; background:var(--c-surface2) !important; }
  .modal-body { padding:14px !important; }
  .close { color:var(--c-muted) !important; opacity:.7 !important; text-shadow:none !important; font-size:16px !important; }

  /* ══ READONLY ════════════════════════════════════════════ */
  .readonly-field { background:var(--c-surface2) !important; border:1px solid var(--c-border) !important; border-radius:4px !important; padding:4px 8px !important; color:var(--c-text) !important; font-size:12px !important; min-height:28px !important; line-height:1.4; }
  .edit-mode-banner { background:rgba(232,160,32,.07) !important; border:1px solid rgba(232,160,32,.25) !important; border-radius:4px !important; padding:7px 12px !important; margin-bottom:10px !important; color:var(--c-accent) !important; font-size:11px !important; font-weight:600 !important; display:flex !important; align-items:center !important; gap:7px !important; }

  /* ══ NOTIFICATIONS ═══════════════════════════════════════ */
  .shiny-notification { background:var(--c-surface) !important; color:var(--c-text) !important; border:1px solid var(--c-border) !important; border-radius:5px !important; box-shadow:0 6px 20px rgba(0,0,0,.4) !important; font-size:12px !important; font-family:'Inter',sans-serif !important; padding:10px 14px !important; }
  .shiny-notification-error   { border-left:3px solid var(--c-danger)  !important; }
  .shiny-notification-warning { border-left:3px solid var(--c-accent)  !important; }
  .shiny-notification-message { border-left:3px solid var(--c-success) !important; }

  /* ══ SCROLLBAR ═══════════════════════════════════════════ */
  ::-webkit-scrollbar { width:5px; height:5px; }
  ::-webkit-scrollbar-track { background:transparent; }
  ::-webkit-scrollbar-thumb { background:var(--c-border); border-radius:3px; }

  /* ══ RESPONSIVE ══════════════════════════════════════════ */
  @media (max-width:767px) {
    .content { padding:10px !important; }
    .submit-bar { flex-direction:column !important; }
    .btn-reset,.btn-submit { width:100% !important; margin:0 0 5px !important; }
  }
"))

common_js <- tags$script(HTML("
  function toggleTheme() {
    var isLight = document.body.classList.contains('theme-light');
    var next = isLight ? 'dark' : 'light';
    document.body.classList.remove('theme-dark','theme-light');
    document.body.classList.add('theme-' + next);
    localStorage.setItem('gp_theme', next);
    Shiny.setInputValue('active_theme', next);
  }
  document.addEventListener('DOMContentLoaded', function() {
    var saved = localStorage.getItem('gp_theme') || 'dark';
    document.body.classList.add('theme-' + saved);
  });
"))

header_toggle <- tags$li(
  class = "dropdown",
  style = "padding:8px 16px; display:flex; align-items:center;",
  tags$span(style="font-size:15px; margin-right:8px;", "🌙"),
  tags$div(class="toggle-track", onclick="toggleTheme()",
           tags$div(class="toggle-thumb")),
  tags$span(style="font-size:15px; margin-left:8px;", "☀️")
)

header_notif <- tags$li(
  class   = "dropdown",
  style   = "padding:8px 16px; cursor:pointer;",
  onclick = "Shiny.setInputValue('open_notifs', Math.random())",
  tags$span("🔔"),
  uiOutput("notif_count_badge", inline = TRUE)
)

sidebar_footer <- tags$div(
  style = "position:absolute; bottom:16px; left:0; right:0;
           padding:12px 16px 0; border-top:1px solid #1a1d2a;",
  actionLink("btn_logout", "🚪 Se déconnecter",
             style = "color:#3a3f52; font-size:11px;")
)

# ── UI ONG ───────────────────────────────────────────────────
ui_ong <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span(
      tags$img(src="https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
               height="26px", style="margin-right:8px;vertical-align:middle;"),
      tags$span("GestionProjets",
                style="font-family:'Playfair Display',serif;font-weight:700;font-size:17px;")
    ),
    titleWidth = 260,
    header_toggle, header_notif,
    tags$li(class="dropdown",
            style="padding:8px 16px; color:#6b7280; font-size:12px;",
            uiOutput("header_user_info"))
  ),
  dashboardSidebar(width=260,
                   sidebarMenu(id="menu_ong",
                               menuItem("📋 Saisie",        tabName="saisie",         icon=icon("edit")),
                               menuItem("🗄️ Historique",    tabName="historique",     icon=icon("history")),
                               menuItem("🔔 Notifications", tabName="notifications",  icon=icon("bell"))
                   ),
                   sidebar_footer
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(common_css, common_js),
    tabItems(
      tabItem(tabName="saisie",         uiOutput("page_saisie")),
      tabItem(tabName="historique",     uiOutput("page_historique")),
      tabItem(tabName="notifications",  uiOutput("page_notifications"))
    )
  )
)

# ── UI ADMIN ─────────────────────────────────────────────────
ui_admin <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span(
      tags$img(src="https://cdn-icons-png.flaticon.com/512/1534/1534938.png",
               height="26px", style="margin-right:8px;vertical-align:middle;"),
      tags$span("GestionProjets — Admin",
                style="font-family:'Playfair Display',serif;font-weight:700;font-size:15px;")
    ),
    titleWidth = 280,
    header_toggle, header_notif,
    tags$li(class="dropdown",
            style="padding:8px 16px; color:#f0c040; font-size:11px; font-weight:600;",
            "👤 ADMIN")
  ),
  dashboardSidebar(width=280,
                   sidebarMenu(id="menu_admin",
                               menuItem("🗓️ Sessions",       tabName="sessions",      icon=icon("calendar")),
                               menuItem("Suivi enquete",     tabName="suivi_enquete", icon=icon("chart-line")),
                               menuItem("Data",              tabName="data",          icon=icon("database")),
                               menuItem("📂 Soumissions",    tabName="soumissions",   icon=icon("folder")),
                               menuItem("✉️ Demandes",       tabName="demandes",      icon=icon("envelope")),
                               menuItem("👥 Utilisateurs",   tabName="utilisateurs",  icon=icon("users")),
                               menuItem("📊 Tableau de bord",tabName="dashboard",     icon=icon("chart-bar")),
                               menuItem("🔔 Notifications",  tabName="notifications", icon=icon("bell"))
                   ),
                   sidebar_footer
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(common_css, common_js),
    tabItems(
      #tabItem(tabName="sessions",      uiOutput("page_sessions")),
      tabItem(tabName="suivi_enquete", uiOutput("page_suivi_enquete")),
      tabItem(tabName="data",          uiOutput("page_data")),
      tabItem(tabName="soumissions",   uiOutput("page_soumissions")),
      tabItem(tabName="demandes",      uiOutput("page_demandes")),
      tabItem(tabName="utilisateurs",  uiOutput("page_utilisateurs")),
      tabItem(tabName="dashboard",     uiOutput("page_dashboard")),
      tabItem(tabName="notifications", uiOutput("page_notifications")),
      tabItem(tabName="sessions",      uiOutput("page_sessions"))
    )
  )
)

# ── UI principale avec shinymanager ──────────────────────────
# On garde un seul dashboardPage au niveau principal. Le menu et les pages
# visibles sont rendus dynamiquement selon le role retourne par shinymanager.
ui <- secure_app(
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = uiOutput("app_title"),
      titleWidth = 280,
      header_toggle,
      header_notif,
      tags$li(
        class = "dropdown",
        style = "padding:8px 16px; color:#6b7280; font-size:12px;",
        uiOutput("header_user_info")
      )
    ),
    dashboardSidebar(
      width = 280,
      uiOutput("sidebar_menu"),
      sidebar_footer
    ),
    dashboardBody(
      useShinyjs(),
      tags$head(common_css, common_js),
      uiOutput("role_body")
    )
  )
)
