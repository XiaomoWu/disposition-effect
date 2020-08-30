# R version: Microsoft R open 3.5.3
# Package version: data.table_1.12.2
#                             stringr_1.4.0
#                             survival_2.43.3
#                             GGally_1.4.0
#                             alpaca_0.3.2
#                             ggplot2_3.3.2

library(data.table)
library(stringr)
library(survival)
library(ggplot2)
library(GGally)
library(alpaca)

# Fig 2
load(file = "sample1.Rdata")
sample1[gain == 1 & pre.follow == 1, state := "pre-follow (gain)"
    ][gain == 0 & pre.follow == 1, state := "pre-follow (loss)"
    ][gain == 1 & post.follow == 1, state := "post-follow (gain)"
    ][gain == 0 & post.follow == 1, state := "post-follow (loss)"]
gg.main <- survfit(Surv(hold.period, sale) ~ state, sample1 = sample1[hold.period < 200 & (pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period))])

d.main <- ggsurv(gg.main,
                             lty.est = c(1, 1, 4, 4),
                             surv.col = c("#CC6666", "#CC6666", "#7777DD", "#7777DD"),
                             plot.cens = F,
                             xlab = "Holding period (days)",
                             ylab = "Remaining position",
                             main = "",
                             size.est = 0.5,
                             order.legend = T
                            ) +
                            theme(
                            legend.title = element_blank(),
                            legend.position = "bottom",
                            legend.spacing.x = unit(0.1, 'cm'),
                            legend.spacing.y = unit(2, 'cm'),
                            plot.margin = unit(c(0, 1, 1, 1), "lines")
                            )

# Fig 3
f.rbst1.early <- sample1[pre.follow == 1]
f.rbst1.late <- sample1[post.follow == 1 & date - as.Date(follow.date) <= pre.period]
DEbeta.e <- f.rbst1.early[, .(pre.follow = glm(sale ~ gain, family = binomial) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst1.late[, .(post.follow = glm(sale ~ gain, family = binomial) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta <- DEbeta.e[DEbeta.l, on = "cube.symbol"]
ggDE <- melt(DEbeta[!is.na(pre.follow) & !is.na(post.follow)], id.vars = "cube.symbol", measure.vars = c("pre.follow", "post.follow"))
setnames(ggDE, 2:3, c("Stage", "DE"))
ggDE[, Stage := as.character(Stage)
    ][, Stage := ifelse(Stage == "pre.follow", "pre-follow", "post-follow")]
rm(DEbeta, DEbeta.e, DEbeta.l, f.rbst1.early, f.rbst1.late)

d.ttest <- ggplot(ggDE, aes(x = DE, colour = Stage, fill = Stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Disposition effect", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    )

# Table 1
descrp.stat <- sample2[, .(cube.symbol, stkcd, date, followings, followers)
    ][sample1, on = .(cube.symbol, stkcd, date), nomatch = 0
    ][pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period), .SD]

ttest <- descrp.stat[, .(trade.num = trd.num[.N],
                                    sale.num = .SD[sale == 1, .N],
                                    sale.gain.num = .SD[sale == 1 & gain == 1, .N],
                                    sale.loss.num = .SD[sale == 1 & gain == 0, .N],
                                    hold.period = mean(hold.period),
                                    hold.period.gain = .SD[gain == 1, mean(hold.period)],
                                    hold.period.loss = .SD[gain == 0, mean(hold.period)],
                                    fans.num = mean(followers),
                                    followings.num = mean(followings)
                                    ),
                                    by = .(cube.symbol, post.follow)
                                    ][, trade.num := ifelse(post.follow == 1, trade.num - shift(trade.num, 1L, "lag", fill = T), trade.num), by = .(cube.symbol)]

# Mean and standard error
aver_sd <- ttest[, as.list(lapply(.SD[, -1], function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))), by = .(post.follow)
    ][, cat := c("mean", "standard error", "mean", "standard error")]

# T-test p-value
t.test.narm <- function(x, y, z) {
    b <- z[!is.na(x) | !is.nan(x), .(x, y)]
    t.test(b$x ~ b$y, data = z) 
}
t_value <- ttest[, lapply(.SD[, - c(1:2, 11)], t.test.narm, post.follow, .SD)
    ][3]

# Table 2
rst.cox.e <- coxph(Surv(hold.period, sale == 1) ~ gain, data = sample1[pre.follow == 1]) %>% summary()
rst.cox.l <- coxph(Surv(hold.period, sale == 1) ~ gain, data = sample1[post.follow == 1 & date - as.Date(follow.date) <= pre.period]) %>% summary()
rst.cox <- coxph(Surv(hold.period, sale == 1) ~ gain + post.follow + I(gain * post.follow), data = sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)]) %>% summary()

# Table 3
rst.main.e <- feglm(sale ~ gain | stkcd + cube.symbol + hold.period, sample1[pre.follow == 1], binomial("logit")) #%>% summary()

rst.main.l <- feglm(sale ~ gain | stkcd + cube.symbol + hold.period, sample1[post.follow == 1 & date - as.Date(follow.date) <= pre.period], binomial("logit")) #%>% summary()

rst.main.t0 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) | stkcd + cube.symbol + hold.period, sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.main.t1 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) + mmt | cube.symbol + stkcd + hold.period, sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.main.t2 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) + as.numeric(active.day / 365) | cube.symbol + stkcd + hold.period, sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.main.t3 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) + as.numeric(trd.num / 1000) | cube.symbol + stkcd + hold.period, sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.main.t4 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stkcd + hold.period, sample1[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.main.f1 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) | cube.symbol + stkcd + hold.period, sample1, binomial("logit")) #%>% summary()

rst.main.f2 <- feglm(sale ~ gain + post.follow + I(gain * post.follow) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stkcd + hold.period, sample1, binomial("logit")) #%>% summary()

# Table 4
load(file = "sample2.Rdata")
rst.ln.i <- feglm(sale ~ gain + I(log(followers + 1)) + I(gain * (log(followers + 1))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stkcd + cube.symbol + hold.period, data = sample2, binomial("logit")) #%>% summary()

rst.ln.o <- feglm(sale ~ gain + I(log(followings + 1)) + I(gain * (log(followings + 1))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stkcd + cube.symbol + hold.period, data = sample2, binomial("logit")) #%>% summary()

rst.ln.lq <- feglm(sale ~ gain + I(cntra * 100) + I(gain * (cntra * 100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stkcd + cube.symbol + hold.period, data = sample2, binomial("logit")) #%>% summary()


rst.ln.f <- feglm(sale ~ gain + I(log(followers + 1)) + I(gain * (log(followers + 1))) + I(log(followings + 1)) + I(gain * (log(followings + 1))) + I(cntra * 100) + I(gain * (cntra * 100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stkcd + cube.symbol + hold.period, data = sample2, binomial("logit")) #%>% summary()

list(rst.ln.i, rst.ln.o, rst.ln.lq, rst.ln.f) %>%
    htmlreg(
            file = "Table4G.doc",
            custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
            custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes"),
                                                 "Holding period FE" = c("Yes", "Yes", "Yes", "Yes"),
                                                 "Stock FE" = c("Yes", "Yes", "Yes", "Yes")),
            custom.coef.names = c("Gain", "In-degree", "Gain * In-degree", "Momentum", "Active days", "Trade number", "Out-degree", "Gain * Out-degree", "Centrality", "Gain * Centrality"),
            reorder.coef = c(1, 2, 3, 7, 8, 9, 10, 4, 5, 6),
            no.margin = T,
            digits = 3
            )