<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2022 Robert Carnell -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes"/>
    <xsl:template match="/">
    <html>
        <head>
            <meta charset="utf-8"/>
            <meta name="description" content="USS Saufley Reunion Photos"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <meta name="robots" content="noindex"/>
            <meta name="copyright" content="2022 Robert Carnell"/>
        
            <!-- US Navy Logo 2022 -->
            <link rel="apple-touch-icon" sizes="180x180" href="images/assets/apple-touch-icon.png"/>
            <link rel="icon" type="image/png" sizes="32x32" href="images/assets/favicon-32x32.png"/>
            <link rel="icon" type="image/png" sizes="16x16" href="images/assets/favicon-16x16.png"/>
                    
            <!-- Latest compiled and minified CSS -->
            <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous"/>
            <!-- Latest compiled and minified JavaScript -->
            <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" integrity="sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" crossorigin="anonymous"></script>
            <!-- jQuery -->
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
        
            <!-- tutorial for amazon S3 photo sharing here:  https://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/s3-example-photos-view.html -->
        
            <style>
              .navbar {
                margin-bottom: 0;
                border-radius: 0;
              }
            </style>
            <title>USS Saufley (DD-465) Reunion Photos</title>
        </head>
        <body>
            <!-- Header -->
            <div id="includeHeader"></div><script>$("#includeHeader").load("navbar1.html");</script>

            <div class="jumbotron-fluid" style="padding-left:15px">
            <h1>USS Saufley DD-465</h1>
            <h3>Reunion Photos</h3>
            </div>
    
            <hr/>

            <div class="container-fluid">
                <h4>Notes:</h4>
                <ul>
                    <li>If you see an error, can name someone pictured, or have a picture from the Saufley Reunions, please enter an issue <a href="https://github.com/bertcarnell/USS-Saufely-DD-465/issues">here</a></li>
                </ul>
                <h4>Locations</h4>
                <div>1954 IN, 1955 OH, 1956 IL, 1957 IL, 1958 OH, 1959 IN</div>
                <div>1960 OH, 1961 WI, 1962 WI, 1963 IL, 1964 CT, 1965 NH, 1966 NC, 1967 --, 1968 VA, 1969 NJ</div>
                <div>1970 PA, 1971 CT, 1972 OH, 1973 RI, 1974 VA, 1975 WI, 1976 FL, 1977 PA, 1978 CA, 1979 RI</div>
                <div>1980 NC, 1981 TX, 1982 CA, 1983 OH, 1984 IA, 1985 MO, 1986 VA, 1987 OH, 1988 FL, 1989 PA</div>
                <div>1990 CT, 1991 MN, 1992 VA, 1993 PL, 1994 PA, 1995 FL, 1996 CO, 1997 MA, 1998 NY, 1999 NV</div>
                <div>2000 MN, 2001 RI, 2002 ??, 2003 CO, 2004 PA, 2005 VA, 2006 CT</div>
            </div>

            <hr/>

            <div id="report_zone">
                <div class="container-fluid">
                    <div class="row">
                        <xsl:for-each select="dataroot/photo">
                            <xsl:variable name="imageurl"><xsl:value-of select="url"/></xsl:variable>
                            <xsl:variable name="alttext">USS Saufley Reunion Photograph</xsl:variable>
                            <div class="col-sm-3">
                                <div class="card" style="width: 18rem;">
                                    <a href="{$imageurl}" target="_blank">
                                        <img src="{$imageurl}" class="card-img-top" alt="{$alttext}"/>
                                    </a>
                                    <div class="card-body">
                                        <h5 class="card-title"><xsl:value-of select="date"/></h5>
                                        <p class="card-text"><xsl:value-of select="location"/></p>
                                        <p class="card-text"><xsl:value-of select="pictured"/></p>
                                        <p class="card-text" style="font-size:50%;"><xsl:value-of select="source"/></p>
                                    </div>
                                </div>
                            </div>
                        </xsl:for-each>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div id="includeFooter"></div><script>$("#includeFooter").load("footer.html");</script>
        </body>
    </html>
    </xsl:template>
</xsl:stylesheet>
