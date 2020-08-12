
# Table of Contents

1.  [Image Storage](#orgadc4dc4)
    1.  [Web application](#orgc255650)
    2.  [Telegram Bot](#org2d47891)
    3.  [Task <code>[3/4]</code>](#org1f48c65)


<a id="orgadc4dc4"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)


<a id="orgc255650"></a>

## Web application


<a id="org2d47891"></a>

## Telegram Bot

-   bind webhook
    -   Manual 
        
            curl "https://api.telegram.org/bot${TOKEN}/setWebhook?url=<PUBLIC_ADDRESS>/bot/webhook/bot${TOKEN}"
-   create *.env.bot* file:
    
        TELEGRAM_TOKEN="<YOUR_TOKEN>"
        TELEGTAM_BOT_NAME="<YOUR_BOT_NAME>"
-   create *.env* file:
    
        WEB_HOST="<YOUR_HOSTNAME>"


<a id="org1f48c65"></a>

## TODO Task <code>[3/4]</code>

-   [X] Categories
    -   [X] Create
    -   [X] Delete
    -   [X] List
-   [X] Reply with keyboard
-   [X] Downloading files
    -   [X] File Download
    -   [X] Integration with inline keyboard
-   [ ] Add more error handling

