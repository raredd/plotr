## add links to ggplot svg image
## http://stackoverflow.com/questions/42259826/hyperlinking-text-in-a-ggplot2-visualization


dd <- within(rawr::rownames_to_column(mtcars, 'car'), {
  link <- c(
    'https://de.wikipedia.org/wiki/AMC_Hornet',
    'https://en.wikipedia.org/wiki/Plymouth_Valiant',
    'https://en.wikipedia.org/wiki/Plymouth_Duster',
    'https://en.wikipedia.org/wiki/Mercedes-Benz_W123'
  )
})[1:4, ]

library('ggplot2')
p <- ggplot(dd, aes(x = mpg, y = car, colour = wt)) + geom_point(size = 5)
ggsave(tf1 <- tempfile(fileext = '.svg'), p)

links <- with(dd, setNames(link, car))

library('xml2')
library('magrittr')
xml <- read_xml(tf1)
xml %>%
  xml_find_all(xpath = '//d1:text') %>% 
  purrr::keep(xml_text(.) %in% names(links)) %>% 
  xml_add_parent('a', 'xlink:href' = links[xml_text(.)], target = '_blank')
write_xml(xml, tf1)


system2(
  'open',
  paste(
    '-a',
    '/applications/Google\\\ Chrome.app',
    tf1
  )
)
