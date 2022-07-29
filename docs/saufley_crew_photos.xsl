<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright 2022 Robert Carnell -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes"/>
    <xsl:template match="/">
    <html>
        <head>
            <meta charset="utf-8"/>
            <meta name="description" content="USS Saufley Crew Photos"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <meta name="robots" content="noindex"/>
            <meta name="copyright" content="2022 Robert Carnell"/>
        
            <!-- US Navy Logo 2022 -->
            <link rel="shortcut icon" type="image/png" href="https://www.cnrc.navy.mil/eToolbox/assets/img/favicon.png"/>
        
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
            <title>USS Saufley (DD-465) Crew Photos</title>
        </head>
        <body>
            <!-- Header -->
            <div id="includeHeader"></div><script>$("#includeHeader").load("navbar1.html");</script>

            <div class="jumbotron-fluid" style="padding-left:15px">
            <h1>USS Saufley DD-465</h1>
            <h3>WWII Crew Photos</h3>
            </div>
    
            <hr/>

            <div class="container-fluid">
                <h4>Notes:</h4>
                <ul>
                    <li>According to Mark Larned in Uncommon Crew, these photographs were taken on Zamboanga, Mindanao, Philippines, March 15, 1945.  See pg 257 of Uncommon Crew.  According to the War Diary, Saufley was in the area on that date, but there is no indication that they were in port or anchored for the photos.</li>
                    <li>In Tin Can Man, EJ Jernigan credits photo 48 to Willis Norman and photo 25 to Orville Elliott</li>
                    <li>Each photo in Robert Rettger's collection was numbered with a stamp on the reverse, likely from the photographer.  Some photos were annotated with the people in the photo by Robert E Rettgers, other descriptions come from various soruces.</li>
                    <li>If you see an error, can name someone pictured, or have a picture from this set of WWII pictures, please enter an issue <a href="https://github.com/bertcarnell/USS-Saufely-DD-465/issues">here</a></li>
                </ul>
            </div>

            <hr/>

            <div id="report_zone">
                <div class="container-fluid">
                    <div class="row">
                        <xsl:for-each select="dataroot/photo">
                            <xsl:variable name="imageurl"><xsl:value-of select="url"/></xsl:variable>
                            <xsl:variable name="alttext">USS Saufley WWII Photograph <xsl:value-of select="title"/></xsl:variable>
                            <div class="col-sm-3">
                                <div class="card" style="width: 18rem;">
                                    <a href="{$imageurl}" target="_blank">
                                        <img src="{$imageurl}" class="card-img-top" alt="{$alttext}"/>
                                    </a>
                                    <div class="card-body">
                                        <xsl:if test="num">
                                            <h5 class="card-title"><em>Photo&#160;<xsl:value-of select="num"/></em>&#160;&#160;<xsl:value-of select="title"/></h5>
                                        </xsl:if>
                                        <xsl:if test="not(num)">
                                            <h5 class="card-title"><xsl:value-of select="title"/></h5>
                                        </xsl:if>
                                        <p class="card-text"><xsl:value-of select="text"/></p>
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
