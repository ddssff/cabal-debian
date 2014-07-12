<VirtualHost *:80>
    ServerAdmin logic@seereason.com
    ServerName www.clckwrks.com
    ServerAlias clckwrks.com

    ErrorLog /var/log/apache2/clckwrks-dot-com-production/error.log
    CustomLog /var/log/apache2/clckwrks-dot-com-production/access.log combined

    ProxyRequests Off
    AllowEncodedSlashes NoDecode

    <Proxy *>
                AddDefaultCharset off
                Order deny,allow
                #Allow from .example.com
                Deny from all
                #Allow from all
    </Proxy>

    <Proxy http://127.0.0.1:9029/*>
                AddDefaultCharset off
                Order deny,allow
                #Allow from .example.com
                #Deny from all
                Allow from all
    </Proxy>

    SetEnv proxy-sendcl 1

    ProxyPass / http://127.0.0.1:9029/ nocanon
    ProxyPassReverse / http://127.0.0.1:9029/
</VirtualHost>
