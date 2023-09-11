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
                <div class="row">
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>1950s</h6></li>
                            <ul>
                                <li>1954  South Bend, IN: Erwin &amp; Helen Hilman</li>
                                <li>1955  Dayton, OH: Herman &amp; Ann Burger</li>
                                <li>1956  Chicago, IL: Ray &amp; Ann Whittington</li>
                                <li>1957  Litchfield, IL: Wynter &amp; Alice Rodgers</li>
                                <li>1958  Columbus, OH: Jim &amp; Mary Fagan</li>
                                <li>1959  South Bend, IN: Erwin &amp; Helen Hillman</li>
                            </ul>
                        </ul>
                    </div>
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>1960s</h6></li>
                            <ul>
                                <li>1960  Dayton, OH: Herman &amp; Ann Burger</li>
                                <li>1961  Waukesha, WI: Joe DeGuiseppe</li>
                                <li>1962  Milwaukee, WI: Clem &amp; Betty Helminiak</li>
                                <li>1963  Rockford, IL: Tom &amp; Florence Curry</li>
                                <li>1964  Norwalk, CT: Fred &amp; Gladys Polesak</li>
                                <li>1965  Alton Bay, NH: Richard &amp; Mary Downing</li>
                                <li>1966  Sylva, NC: Andy &amp; Faye Parker</li>
                                <li>1967</li>
                                <li>1968  Chester, VA: Emory (EJ) &amp; Ann Jernigan</li>
                                <li>1969  Middletown, NJ: Robert &amp; Verna Vordick</li>
                            </ul>
                        </ul>
                    </div>
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>1970s</h6></li>
                            <ul>
                                <li>1970  Bethlehem, PA: Vernon &amp; Lois Ebling</li>
                                <li>1971  Wolcott, CT:  Arthur &amp; Anita Cyr</li>
                                <li>1972  Newberg Hts, OH: Ray &amp; Helen DeCenzi</li>
                                <li>1973  Newport, RI: Joseph &amp; Alice Devine</li>
                                <li>1974  Chester, VA: Emory (EJ) &amp; Ann Jernigan</li>
                                <li>1975  Waukesha, WI:  Joe &amp; Laura DeGuiseppe</li>
                                <li>1976  Longboat Key, FL: Herman &amp; Ann Burger</li>
                                <li>1977  Fleetwood, PA: Ralph &amp; Hanna DeHart</li>
                                <li>1978  Tustin, CA: Chester &amp; Marion Vetter</li>
                                <li>1979  Rumford, RI: John &amp; Barbara Larned</li>
                            </ul>
                        </ul>
                    </div>
                </div>
                <div class="row">
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>1980s</h6></li>
                            <ul>
                                <li>1980  Newton, NC: Carl &amp; Madge Watts</li>
                                <li>1981  Dallas, TX:  Karl &amp; Jane Braddick</li>
                                <li>1982  Covina, CA: Jim &amp; Marie Hartley, Randolph (Tiny) &amp; Lou Meadows</li>
                                <li>1983  Maple Hts, OH: Tony &amp; Shirley Eabon</li>
                                <li>1984  Waterloo, IA: Kennison &amp; Mildered Hayes</li>
                                <li>1985  Independence, MO: Willis &amp; Blanche Norman</li>
                                <li>1986  Lynchburg, VA: Jayson &amp; Margaret Tomlinson</li>
                                <li>1987  Struthers, OH: Stephen &amp; Loretta Macala</li>
                                <li>1988  Dayton Shores, FL: Robert &amp; Laura Cromie</li>
                                <li>1989  Philadelphia, PA: Daniel &amp; Monti Michie</li>
                            </ul>
                        </ul>
                    </div>
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>1990s</h6></li>
                            <ul>
                                <li>1990  New London, CT: William &amp; Barbara Downie</li>
                                <li>1991  Bloomington, MN: Charles &amp; Margaret McLellan, Warren &amp; Lorraine Heyne</li>
                                <li>1992  Fairfax City, VA: Emory (EJ) Jernigan</li>
                                <li>1993  Daytona Shores, FL: Robert &amp; Laura Cromie</li>
                                <li>1994  Harrisburg, PA: Robert &amp; Marge Rettgers</li>
                                <li>1995  Daytona Shores, FL: Robert &amp; Laura Cromie</li>
                                <li>1996  Colorado Springs, CO: Charles &amp; Margaret McLellan, Warren &amp; Lorraine Heyne</li>
                                <li>1997  Great Barrington, MA: Fred &amp; Joan Polesak</li>
                                <li>1998  Grand Island, NY: Harold &amp; Gerrie Hutchinson</li>
                                <li>1999  Reno, NE:  Bill &amp; Barbara Downie</li>
                            </ul>
                        </ul>
                    </div>
                    <div class="col-sm-4">
                        <ul>
                            <li><h6>2000s</h6></li>
                            <ul>
                                <li>2000  Minneapolis, MN: Warren &amp; Lorraine Heyne</li>
                                <li>2001  Middletown, RI: John Larned &amp; Family</li>
                                <li>2002  Daytona Shores, FL: Laura Cromie, Harold &amp; Gerrie Hutchinson</li>
                                <li>2003  Colorado Springs, CO: Warren &amp; Lorraine Heyne</li>
                                <li>2004  Pittsburg, PA: John Wszeborowski</li>
                                <li>2005  Virginia Beach, VA: Christina Cochran DeCaravjal, John &amp; Bob Larned</li>
                                <li>2006  New London, CT: William &amp; Barbara Downie, Maggie Downie</li>
                                <li>2007  Pittsburg, PA: John Wszeborowski</li>
                                <li>2008  Colorado Spings, CO: Warren Heyne</li>
                            </ul>
                        </ul>
                    </div>
                </div>
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
