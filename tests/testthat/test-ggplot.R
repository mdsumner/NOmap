context("test ggplot stuff")

test_that("SOgg returns a ggplot object", {
    ## use the full SOmap2 set of layer options here, just so all code is exercised
    p <- SOmap2(Trim = -45, Grats = TRUE, fronts = TRUE, MPA = TRUE, MPAlab = TRUE,
                CCAMLR = TRUE, CCAMLRlab = TRUE, SSRU = TRUE, SSRUlab = TRUE,
                SSMU = TRUE, SSMUlab = TRUE, RB = TRUE, RBlab = TRUE,
                SPRFMORB = TRUE, EEZ = TRUE, EEZlab = TRUE,
                Domains = TRUE, Domainslab = TRUE, IWC = TRUE, IWClab = TRUE)
    pg <- SOgg(p)
    expect_s3_class(pg, "ggplot")
})
