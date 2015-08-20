<VirtualHost *:80>
    ServerAdmin logic@seereason.com
    ServerName www.my.appraisalreportonline.com
    ServerAlias my.appraisalreportonline.com

    ErrorLog /var/log/apache2/artvaluereport2-production/error.log
    CustomLog /var/log/apache2/artvaluereport2-production/access.log combined

    ProxyRequests Off
    AllowEncodedSlashes NoDecode

    <Proxy *>
                AddDefaultCharset off
                Order deny,allow
                #Allow from .example.com
                Deny from all
                #Allow from all
    </Proxy>

    <Proxy http://127.0.0.1:9027/*>
                AddDefaultCharset off
                Order deny,allow
                #Allow from .example.com
                #Deny from all
                Allow from all
    </Proxy>

    SetEnv proxy-sendcl 1

    ProxyPass / http://127.0.0.1:9027/ nocanon
    ProxyPassReverse / http://127.0.0.1:9027/
</VirtualHost>
