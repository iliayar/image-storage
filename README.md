
# Table of Contents

1.  [Image Storage](#orgb98d4e2)
    1.  [Web application](#orgd98ace7)
        1.  [Tasks <code>[0/1]</code>](#org0349a6c)
    2.  [Telegram Bot](#org62ec79f)
    3.  [Task <code>[3/4]</code>](#orgbd7ce4c)


<a id="orgb98d4e2"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)


<a id="orgd98ace7"></a>

## Web application


<a id="org0349a6c"></a>

### TODO Tasks <code>[0/1]</code>

-   [ ] Add styles?


<a id="org62ec79f"></a>

## Telegram Bot

-   bind webhook
    -   Manual 
        
            curl "https://api.telegram.org/bot${TOKEN}/setWebhook?url=<PUBLIC_ADDRESS>/bot/webhook/bot${TOKEN}"
-   create *.env.bot* file:
    
        TELEGRAM_TOKEN="<YOUR_TOKEN>"
        TELEGTAM_BOT_NAME="<YOUR_BOT_NAME>"
-   create *.env* file:
    
        WEB_HOST="<YOUR_HOSTNAME>"


<a id="orgbd7ce4c"></a>

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

