
# Table of Contents

1.  [Image Storage](#orge9799ad)
    1.  [Web application](#org369ed6b)
        1.  [Tasks <code>[1/1]</code>](#orgab5565e)
    2.  [Telegram Bot](#orgdeecddf)
        1.  [Tasks <code>[3/4]</code>](#org21867dd)


<a id="orge9799ad"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)


<a id="org369ed6b"></a>

## Web application


<a id="orgab5565e"></a>

### TODO Tasks <code>[1/1]</code>

-   [X] Add styles?


<a id="orgdeecddf"></a>

## Telegram Bot

-   bind webhook
    -   Manual 
        
            curl "https://api.telegram.org/bot${TOKEN}/setWebhook?url=<PUBLIC_ADDRESS>/bot/webhook/bot${TOKEN}"
-   create *.env.bot* file:
    
        TELEGRAM_TOKEN="<YOUR_TOKEN>"
        TELEGTAM_BOT_NAME="<YOUR_BOT_NAME>"
-   create *.env* file:
    
        WEB_HOST="<YOUR_HOSTNAME>"


<a id="org21867dd"></a>

### TODO Tasks <code>[3/4]</code>

-   [X] Categories
    -   [X] Create
    -   [X] Delete
    -   [X] List
-   [X] Reply with keyboard
-   [X] Downloading files
    -   [X] File Download
    -   [X] Integration with inline keyboard
-   [ ] Add more error handling

