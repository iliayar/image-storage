
# Table of Contents

1.  [Image Storage](#org559b107)
    1.  [Web application](#org77d2719)
        1.  [Tasks <code>[0/1]</code>](#org9cc2026)
    2.  [Telegram Bot](#orgb2476bf)
        1.  [Tasks <code>[3/4]</code>](#org0cd7d25)


<a id="org559b107"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)


<a id="org77d2719"></a>

## Web application


<a id="org9cc2026"></a>

### TODO Tasks <code>[0/1]</code>

-   [ ] Add styles?


<a id="orgb2476bf"></a>

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


<a id="org0cd7d25"></a>

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

