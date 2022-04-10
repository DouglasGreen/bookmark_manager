:- module(url, [url/6]).

% url(URL, Count, Code, Date, Title, Description).

:- dynamic url:url/6.

url:url("https://microsoft.com",
        2,
        200,
        date(2022, 4, 10),
        'Microsoft – Cloud, Computers, Apps & Gaming',
        'Explore Microsoft products and services for your home or business. Shop Surface, Microsoft 365, Xbox, Windows, Azure, and more. Find downloads and get support.').
url:url("https://linkedin.com",
        2,
        200,
        date(2022, 4, 10),
        'LinkedIn: Log In or Sign Up',
        '750 million+ members | Manage your professional identity. Build and engage with your professional network. Access knowledge, insights and opportunities.').
url:url("https://bing.com",
        3,
        200,
        date(2022, 4, 10),
        'Bing',
        'Bing helps you turn information into action, making it faster and easier to go from searching to doing.').
url:url("https://duckduckgo.com",
        2,
        200,
        date(2022, 4, 10),
        'DuckDuckGo — Privacy, simplified.',
        'The Internet privacy company that empowers you to seamlessly take control of your personal information online, without any tradeoffs.').
url:url("https://yahoo.com",
        1,
        200,
        date(2022, 4, 10),
        'Yahoo | Mail, Weather, Search, Politics, News, Finance, Sports & Videos',
        'Latest news coverage, email, free stock quotes, live scores and video are just the beginning. Discover more every day at Yahoo!').
url:url("https://reddit.com",
        1,
        200,
        date(2022, 4, 10),
        'Reddit - Dive into anything',
        'Reddit is a network of communities where people can dive into their interests, hobbies and passions. There\'s a community for whatever you\'re interested in on Reddit.').
url:url("https://twitter.com", 2, 200, date(2022, 4, 10), no, no).
url:url("https://walmart.com",
        1,
        200,
        date(2022, 4, 10),
        'Walmart.com | Save Money. Live Better',
        'Shop Walmart.com today for Every Day Low Prices. Join Walmart+ for unlimited free delivery from your store & free shipping with no order minimum. Start your free 30-day trial now!').
url:url("https://whatsapp.com", 2, 200, date(2022, 4, 10), 'WhatsApp', no).
url:url("https://google.com",
        1,
        200,
        date(2022, 4, 10),
        'Google',
        'Search the world\'s information, including webpages, images, videos and more. Google has many special features to help you find exactly what you\'re looking for.').
url:url("https://live.com",
        1,
        200,
        date(2022, 4, 10),
        'Outlook – free personal email and calendar from Microsoft',
        'Get free Outlook email and calendar, plus Office Online apps like Word, Excel and PowerPoint. Sign in to access your Outlook, Hotmail or Live email account.').
url:url("https://ebay.com",
        1,
        200,
        date(2022, 4, 10),
        'Electronics, Cars, Fashion, Collectibles & More | eBay',
        'Buy & sell electronics, cars, clothes, collectibles & more on eBay, the world&apos;s online marketplace. Top brands, low prices & free shipping on many items.').
url:url("https://facebook.com",
        2,
        200,
        date(2022, 4, 10),
        'Facebook - Log In or Sign Up',
        'Log into Facebook to start sharing and connecting with your friends, family, and people you know.').
url:url("https://instagram.com",
        1,
        200,
        date(2022, 4, 10),
        'Instagram',
        'Create an account or log in to Instagram - A simple, fun & creative way to capture, edit & share photos, videos & messages with friends & family.').
url:url("https://cnn.com",
        3,
        200,
        date(2022, 4, 10),
        'CNN - Breaking News, Latest News and Videos',
        'View the latest news and breaking news today for U.S., world, weather, entertainment, politics and health at CNN.com.').
url:url("https://amazon.com",
        1,
        200,
        date(2022, 4, 10),
        'Amazon.com. Spend less. Smile more.',
        'Free shipping on millions of items. Get the best of Shopping and Entertainment with Prime. Enjoy low prices and great deals on the largest selection of everyday essentials and other products, including fashion, home, beauty, electronics, Alexa Devices, sporting goods, toys, automotive, pets, baby, books, video games, musical instruments, office supplies, and more.').
url:url("https://vk.com",
        1,
        200,
        date(2022, 4, 10),
        'VK mobile version | VK',
        'VK is the largest European social network with more than 100 million active users. Our goal is to keep old friends, ex-classmates, neighbors and colleagues in touch.').
url:url("https://aliexpress.com",
        1,
        200,
        date(2022, 4, 10),
        'AliExpress - Online Shopping for Popular Electronics, Fashion, Home & Garden, Toys & Sports, Automobiles and More products  - AliExpress',
        'Passion shouldn\'t cost a fortune. On AliExpress, shop online for over 111 million quality deals on Fashion, Accessories, Computer Electronics, Toys, Tools, Home Improvement, Home Appliances, Home & Garden and more!').
url:url("https://paypal.com",
        1,
        200,
        date(2022, 4, 10),
        'Send Money, Pay Online or Set Up a Merchant Account - PayPal',
        'PayPal is the faster, safer way to send money, make an online payment, receive money or set up a merchant account.').
url:url("https://msn.com",
        1,
        200,
        date(2022, 4, 10),
        'MSN | Outlook, Office, Skype, Bing, Breaking News, and Latest Videos',
        'Your customizable and curated collection of the best in trusted news plus coverage of sports, entertainment, money, weather, travel, health and lifestyle, combined with Outlook/Hotmail, Facebook, Twitter, Bing, Skype and more.').
url:url("https://discord.com",
        1,
        200,
        date(2022, 4, 10),
        'Discord | Your Place to Talk and Hang Out',
        'Discord is the easiest way to talk over voice, video, and text. Talk, chat, hang out, and stay close with your friends and communities.').
url:url("https://zoom.us",
        1,
        200,
        date(2022, 4, 10),
        'Video Conferencing, Cloud Phone, Webinars, Chat, Virtual Events | Zoom',
        'Zoom\'s secure, reliable video platform powers all of your communication needs, including meetings, chat, phone, webinars, and online events.').
url:url("https://wikipedia.org",
        2,
        200,
        date(2022, 4, 10),
        'Wikipedia',
        'Wikipedia is a free online encyclopedia, created and edited by volunteers around the world and hosted by the Wikimedia Foundation.').
url:url("https://twitch.tv",
        1,
        200,
        date(2022, 4, 10),
        'Twitch',
        'Twitch is an interactive livestreaming service for content spanning gaming, entertainment, sports, music, and more. There’s something for everyone on Twitch.').
url:url("https://youtube.com",
        1,
        200,
        date(2022, 4, 10),
        'YouTube',
        'Enjoy the videos and music you love, upload original content, and share it all with friends, family, and the world on YouTube.').
url:url("https://office.com",
        1,
        200,
        date(2022, 4, 10),
        'Office 365 Login | Microsoft Office',
        'Collaborate for free with online versions of Microsoft Word, PowerPoint, Excel, and OneNote. Save documents, workbooks, and presentations online, in OneDrive. Share them with others and work together at the same time.').
url:url("https://pinterest.com", 1, 308, date(2022, 4, 10), 'Permanent Redirect', no).

