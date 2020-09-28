
# Table of Contents

1.  [Image Storage](#org34cbeae)
    1.  [Web application](#orga6069e8)
        1.  [Tasks <code>[0/1]</code>](#org5420655)
    2.  [Telegram Bot](#org0ca9b7a)
        1.  [Tasks <code>[3/4]</code>](#orgaeaae5f)


<a id="org34cbeae"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)   
Available here [Image Storage](https://t.me/imagestoragebot)


<a id="orga6069e8"></a>

## Web application


<a id="org5420655"></a>

### TODO Tasks <code>[0/1]</code>

-   [ ] Add styles?


<a id="org0ca9b7a"></a>

## Telegram Bot

-   bind webhook
    -   Manual 
        
            curl "https://api.telegram.org/bot${TOKEN}/setWebhook?url=<PUBLIC_ADDRESS>/bot/webhook/bot${TOKEN}"
-   create *.env.bot* file:
    
        TELEGRAM_TOKEN="<YOUR_TOKEN>"
        TELEGRAM_BOT_NAME="<YOUR_BOT_NAME>"
-   create *.env* file:
    
        WEB_HOST="<YOUR_HOSTNAME>"

**In .env files, on ubuntu double qoutes includes in variable**


<a id="orgaeaae5f"></a>

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

