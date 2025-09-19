# R/utils.R - Utility functions

# Team constants
NFL_TEAMS <- list(
  afc = c("BAL", "BUF", "CIN", "CLE", "DEN", "HOU", "IND", "JAX",
          "KC", "LAC", "LV", "MIA", "NE", "NYJ", "PIT", "TEN"),
  nfc = c("ARI", "ATL", "CAR", "CHI", "DAL", "DET", "GB", "LAR",
          "MIN", "NO", "NYG", "PHI", "SEA", "SF", "TB", "WAS"),
  all = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
          "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
          "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
          "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")
)

# Team name mapping
TEAM_NAMES <- c(
  "ARI" = "Arizona Cardinals", "ATL" = "Atlanta Falcons",
  "BAL" = "Baltimore Ravens", "BUF" = "Buffalo Bills",
  "CAR" = "Carolina Panthers", "CHI" = "Chicago Bears",
  "CIN" = "Cincinnati Bengals", "CLE" = "Cleveland Browns",
  "DAL" = "Dallas Cowboys", "DEN" = "Denver Broncos",
  "DET" = "Detroit Lions", "GB" = "Green Bay Packers",
  "HOU" = "Houston Texans", "IND" = "Indianapolis Colts",
  "JAX" = "Jacksonville Jaguars", "KC" = "Kansas City Chiefs",
  "LAC" = "Los Angeles Chargers", "LAR" = "Los Angeles Rams",
  "LV" = "Las Vegas Raiders", "MIA" = "Miami Dolphins",
  "MIN" = "Minnesota Vikings", "NE" = "New England Patriots",
  "NO" = "New Orleans Saints", "NYG" = "New York Giants",
  "NYJ" = "New York Jets", "PHI" = "Philadelphia Eagles",
  "PIT" = "Pittsburgh Steelers", "SEA" = "Seattle Seahawks",
  "SF" = "San Francisco 49ers", "TB" = "Tampa Bay Buccaneers",
  "TEN" = "Tennessee Titans", "WAS" = "Washington Commanders"
)

# Get full team name
get_team_name <- function(abbr) {
  if (abbr %in% names(TEAM_NAMES)) {
    TEAM_NAMES[[abbr]]
  } else {
    abbr
  }
}

# Standardize team names
standardize_team_names <- function(team) {
  case_when(
    team == "LA" ~ "LAR",
    team == "STL" ~ "LAR",
    team == "SD" ~ "LAC",
    team == "OAK" ~ "LV",
    TRUE ~ team
  )
}

# Plot to base64 conversion - Fixed version
plot_to_base64 <- function(plot, width = 12, height = 8, dpi = 300) {
  temp_file <- tempfile(fileext = ".png")
  
  showtext_auto(enable = TRUE)
  showtext_opts(dpi = dpi)
  
  # Try different PNG devices based on availability
  tryCatch({
    # Try with ragg if available (usually better quality)
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(
        filename = temp_file,
        width = width,
        height = height,
        units = "in",
        res = dpi,
        background = "white"
      )
    } else {
      # Fall back to standard png without Cairo
      png(
        filename = temp_file,
        width = width * dpi,
        height = height * dpi,
        res = dpi,
        bg = "white"
      )
    }
    
    print(plot)
    dev.off()
    
    base64_string <- base64encode(temp_file)
    unlink(temp_file)
    
    return(base64_string)
    
  }, error = function(e) {
    # If all else fails, try with grDevices::png() with basic settings
    png(
      filename = temp_file,
      width = width * dpi,
      height = height * dpi,
      res = dpi,
      bg = "white"
    )
    
    print(plot)
    dev.off()
    
    base64_string <- base64encode(temp_file)
    unlink(temp_file)
    
    return(base64_string)
  })
}

# Screenshot JavaScript
screenshot_js <- function() {
  "
  function downloadTableImage(namespaceId, containerId) {
    const element = document.getElementById(namespaceId + containerId);
    if (!element) return;
    
    const btn = event.target || event.srcElement;
    const originalText = btn.innerText;
    btn.innerText = 'Generating...';
    btn.disabled = true;
    
    const rect = element.getBoundingClientRect();
    
    html2canvas(element, {
      scale: 2,
      backgroundColor: '#ffffff',
      logging: false,
      useCORS: true,
      allowTaint: true,
      width: rect.width,
      height: rect.height
    }).then(canvas => {
      canvas.toBlob(function(blob) {
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = containerId + '_' + new Date().toISOString().split('T')[0] + '.png';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
        
        btn.innerText = originalText;
        btn.disabled = false;
      });
    }).catch(error => {
      btn.innerText = originalText;
      btn.disabled = false;
    });
  }
  "
}

# Custom CSS
xfp_custom_css <- function() {
  paste0("
    /* Player chip styles - redesigned as toggle buttons */
    .player-chip {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      height: 42px;
      padding: 8px 16px;
      background: #f8f9fa;
      color: #495057;
      border: 1px solid #ced4da;
      border-radius: 6px;
      font-weight: 600;
      font-size: 22px;
      transition: all 0.15s;
      cursor: pointer;
      user-select: none;
      font-family: 'Inter', sans-serif;
      position: relative;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    .player-chip:hover {
      background: #e9ecef;
      transform: translateY(-1px);
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    .player-chip.selected {
      background: white;
      color: #333;
      border: 2px solid ", etr_colors$primary, ";
      box-shadow: 0 0 0 3px rgba(55, 175, 74, 0.15);
      padding: 7px 15px;
    }
    
    .player-chip.selected:hover {
      box-shadow: 0 0 0 4px rgba(55, 175, 74, 0.25);
      transform: translateY(-1px);
    }
    
    .player-chip.deselected {
      background: #f8f9fa;
      color: #495057;
      border: 1px solid #ced4da;
      opacity: 0.8;
    }
    
    .player-chip.deselected:hover {
      opacity: 1;
      background: #e9ecef;
    }
    
    .player-chip .position-badge {
      display: none;
    }
    
    .player-chip .player-name {
      font-weight: 600;
      font-size: 22px;
      margin: 0;
      padding: 0;
    }
    
    .player-chip .player-xfp {
      display: none;
    }
    
    .player-chip .check-icon {
      display: none;
    }
    
    /* Stats value styling */
    .stats-value {
      font-family: 'Inter', sans-serif !important;
      font-weight: ", etr_fonts$weight_medium, ";
      font-size: 26px;
      text-align: center;
      display: flex;
      align-items: center;
      justify-content: center;
      height: 60px;
      width: 100%;
    }
    
    /* Bar chart styles */
    .bar-chart-cell {
      position: relative;
      width: 100%;
      height: 60px;
      display: flex;
      align-items: center;
      padding: 0 10px;
    }
    
    .bar-container {
      position: relative;
      width: 100%;
      max-width: 400px;
      height: 40px;
    }
    
    .expected-bar {
      position: absolute;
      height: 35px;
      background-color: transparent;
      border: 3px solid ", etr_colors$gray_800, ";
      border-radius: 0;
      top: 2px;
      left: 0;
      box-sizing: border-box;
      z-index: 2;
    }
    
    .actual-bar {
      position: absolute;
      height: 35px;
      background-color: ", etr_colors$primary, ";
      border-radius: 0;
      top: 2px;
      left: 0;
      z-index: 1;
    }
    
    .bar-label {
      position: absolute;
      font-family: 'Inter', sans-serif !important;
      font-weight: bold;
      font-size: 16px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      padding: 2px 6px;
      white-space: nowrap;
    }
    
    .expected-label {
      position: absolute;
      right: 8px;
      top: -10px;
      color: ", etr_colors$gray_800, ";
      background-color: transparent;
      text-shadow: 
        0 0 8px white, 0 0 8px white, 0 0 8px white, 0 0 8px white,
        0 0 10px white, 0 0 10px white, 0 0 12px white, 0 0 12px white,
        1px 1px 3px white, -1px -1px 3px white, 1px -1px 3px white, -1px 1px 3px white;
      z-index: 3;
      font-weight: ", etr_fonts$weight_bold, ";
      font-size: 16px;
      line-height: 20px;
      font-family: 'Inter', sans-serif !important;
    }
    
    .actual-label {
      color: ", etr_colors$gray_50, ";
      z-index: 4;
      top: 8px;
      left: 8px;
    }
    
    /* Force Inter on all reactable cells */
    .rt-td {
      font-family: 'Inter', sans-serif !important;
    }
    
    /* Team logo styles */
    .logo-row {
      display: flex;
      justify-content: center;
      gap: 6px;
      margin-bottom: 10px;
      flex-wrap: wrap;
    }
    
    .team-logo {
      position: relative;
      display: inline-block;
      cursor: pointer;
    }
    
    .team-logo img {
      width: 40px;
      height: 40px;
      object-fit: contain;
      transition: all 0.2s ease;
      border: 2px solid transparent;
      border-radius: 4px;
      filter: grayscale(100%);
      opacity: 0.7;
    }
    
    .team-logo:hover img {
      opacity: 1;
      transform: scale(1.05);
    }
    
    .team-logo.selected img {
      transform: scale(1.1);
      filter: none;
      border-color: ", etr_colors$gray_800, ";
      opacity: 1;
    }
    
    /* Week selector styles */
    .week-selector-box {
      transition: all 0.15s ease !important;
    }
    
    .week-selector-box:not(.selected) {
      background: ", etr_colors$gray_100, " !important;
      color: ", etr_colors$gray_600, " !important;
      border: 1px solid ", etr_colors$gray_300, " !important;
      opacity: 0.6 !important;
    }
    
    .week-selector-box.selected {
      background: ", etr_colors$primary, " !important;
      color: white !important;
      border: 1px solid ", etr_colors$primary_dark, " !important;
      opacity: 1 !important;
    }
  ")
}