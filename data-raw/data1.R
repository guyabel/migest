ur_


numbers2 <- tesseract(options = list(tessedit_char_whitelist = "-0123456789"))
text <- tesseract::ocr("./data-raw/un1986-tab8.png", engine = numbers2)
cat(text)


p1 <- tesseract(options = list(tessedit_char_whitelist = "-0123456789 "))
text <- tesseract::ocr("./data-raw/plane1981-tab1.png")
text <- tesseract::ocr("./data-raw/plane1981-tab2.png")
cat(text)
