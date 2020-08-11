
# Table of Contents

1.  [Image Storage](#org7548dde)
    1.  [Web application](#orgf7b6e44)
    2.  [Telegram Bot](#org141f02c)
    3.  [Task <code>[3/4]</code>](#orga2e6a5b)


<a id="org7548dde"></a>

# Image Storage

![img](https://github.com/iliayar/image-storage/workflows/build/badge.svg)


<a id="orgf7b6e44"></a>

## Web application


<a id="org141f02c"></a>

## Telegram Bot

-   bind webhook
    -   Manual 
        
            curl "https://api.telegram.org/bot${TOKEN}/setWebhook?url=<PUBLIC_ADDRESS>/bot/webhook/bot${TOKEN}"
-   create *.env* file:
    
        TELEGRAM_TOKEN="<YOUR_TOKEN>"


<a id="orga2e6a5b"></a>

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

