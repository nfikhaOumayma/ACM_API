/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link LoanAnalyticsDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.1.3
 */
public class LoanAnalyticsDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8070369452076184827L;

	/** The total. */
	private Float total;

	/** The pourcentage. */
	private Float pourcentage;

	/** The loan number by product: Format : Arrays Integer : [44, 55]. */
	private List<Integer> loanNumberByProduct;

	/** The labels products: Format : Arrays String ['Product A', 'Product B']. */
	private List<String> labelsProducts;

	/** The xaxis categories. */
	private List<String> xaxisCategories;

	/** The series applied loans. */
	private List<Integer> seriesAppliedLoans;

	/** The series approved loans. */
	private List<Integer> seriesApprovedLoans;

	/** The series canceled rejected loans. */
	private List<Integer> seriesCanceledRejectedLoans;

	/** The currency. */
	private String currency;

	/** The total customers. */
	private Integer totalCustomers;

	/** The total actives customers. */
	private Long totalActivesCustomers;

	/** The series amount loans. */
	private List<Long> seriesAmountLoans;

	/** The series total customers. */
	private List<Integer> seriesTotalCustomers;

	/** The series total active customers. */
	private List<Integer> seriesTotalActiveCustomers;

	/**
	 * Instantiates a new loan analytics DTO.
	 */
	public LoanAnalyticsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loan analytics DTO : For General Stats.
	 *
	 * @param total the total
	 * @param pourcentage the pourcentage
	 */
	public LoanAnalyticsDTO(Float total, Float pourcentage) {

		this.total = total;
		this.pourcentage = pourcentage;
	}

	/**
	 * Instantiates a new loan analytics DTO : For Donut Chart.
	 *
	 * @param loanNumberByProduct the loan number by product
	 * @param labelsProducts the labels products
	 */
	public LoanAnalyticsDTO(List<Integer> loanNumberByProduct, List<String> labelsProducts) {

		this.loanNumberByProduct = loanNumberByProduct;
		this.labelsProducts = labelsProducts;
	}

	/**
	 * Instantiates a new loan analytics DTO : For Bar chart.
	 *
	 * @param xaxisCategories the xaxis categories
	 * @param seriesAppliedLoans the series applied loans
	 * @param seriesApprovedLoans the series approved loans
	 * @param seriesCanceledRejectedLoans the series canceled rejected loans
	 */
	public LoanAnalyticsDTO(List<String> xaxisCategories, List<Integer> seriesAppliedLoans,
			List<Integer> seriesApprovedLoans, List<Integer> seriesCanceledRejectedLoans) {

		this.xaxisCategories = xaxisCategories;
		this.seriesAppliedLoans = seriesAppliedLoans;
		this.seriesApprovedLoans = seriesApprovedLoans;
		this.seriesCanceledRejectedLoans = seriesCanceledRejectedLoans;
	}

	/**
	 * Instantiates a new loan analytics DTO.
	 *
	 * @param total the total
	 * @param currency the currency
	 */
	public LoanAnalyticsDTO(Float total, String currency) {

		this.total = total;
		this.currency = currency;
	}

	/**
	 * Gets the total.
	 *
	 * @return the total
	 */
	public Float getTotal() {

		return total;
	}

	/**
	 * Sets the total.
	 *
	 * @param total the total to set
	 */
	public void setTotal(Float total) {

		this.total = total;
	}

	/**
	 * Gets the pourcentage.
	 *
	 * @return the pourcentage
	 */
	public Float getPourcentage() {

		return pourcentage;
	}

	/**
	 * Sets the pourcentage.
	 *
	 * @param pourcentage the pourcentage to set
	 */
	public void setPourcentage(Float pourcentage) {

		this.pourcentage = pourcentage;
	}

	/**
	 * Gets the loan number by product.
	 *
	 * @return the loanNumberByProduct
	 */
	public List<Integer> getLoanNumberByProduct() {

		return loanNumberByProduct;
	}

	/**
	 * Sets the loan number by product.
	 *
	 * @param loanNumberByProduct the loanNumberByProduct to set
	 */
	public void setLoanNumberByProduct(List<Integer> loanNumberByProduct) {

		this.loanNumberByProduct = loanNumberByProduct;
	}

	/**
	 * Gets the labels products.
	 *
	 * @return the labelsProducts
	 */
	public List<String> getLabelsProducts() {

		return labelsProducts;
	}

	/**
	 * Sets the labels products.
	 *
	 * @param labelsProducts the labelsProducts to set
	 */
	public void setLabelsProducts(List<String> labelsProducts) {

		this.labelsProducts = labelsProducts;
	}

	/**
	 * Gets the xaxis categories.
	 *
	 * @return the xaxisCategories
	 */
	public List<String> getXaxisCategories() {

		return xaxisCategories;
	}

	/**
	 * Sets the xaxis categories.
	 *
	 * @param xaxisCategories the xaxisCategories to set
	 */
	public void setXaxisCategories(List<String> xaxisCategories) {

		this.xaxisCategories = xaxisCategories;
	}

	/**
	 * Gets the series applied loans.
	 *
	 * @return the seriesAppliedLoans
	 */
	public List<Integer> getSeriesAppliedLoans() {

		return seriesAppliedLoans;
	}

	/**
	 * Sets the series applied loans.
	 *
	 * @param seriesAppliedLoans the seriesAppliedLoans to set
	 */
	public void setSeriesAppliedLoans(List<Integer> seriesAppliedLoans) {

		this.seriesAppliedLoans = seriesAppliedLoans;
	}

	/**
	 * Gets the series approved loans.
	 *
	 * @return the seriesApprovedLoans
	 */
	public List<Integer> getSeriesApprovedLoans() {

		return seriesApprovedLoans;
	}

	/**
	 * Sets the series approved loans.
	 *
	 * @param seriesApprovedLoans the seriesApprovedLoans to set
	 */
	public void setSeriesApprovedLoans(List<Integer> seriesApprovedLoans) {

		this.seriesApprovedLoans = seriesApprovedLoans;
	}

	/**
	 * Gets the series canceled rejected loans.
	 *
	 * @return the seriesCanceledRejectedLoans
	 */
	public List<Integer> getSeriesCanceledRejectedLoans() {

		return seriesCanceledRejectedLoans;
	}

	/**
	 * Sets the series canceled rejected loans.
	 *
	 * @param seriesCanceledRejectedLoans the seriesCanceledRejectedLoans to set
	 */
	public void setSeriesCanceledRejectedLoans(List<Integer> seriesCanceledRejectedLoans) {

		this.seriesCanceledRejectedLoans = seriesCanceledRejectedLoans;
	}

	/**
	 * Gets the currency.
	 *
	 * @return the currency
	 */
	public String getCurrency() {

		return currency;
	}

	/**
	 * Sets the currency.
	 *
	 * @param currency the currency to set
	 */
	public void setCurrency(String currency) {

		this.currency = currency;
	}

	/**
	 * Gets the total customers.
	 *
	 * @return the totalCustomers
	 */
	public Integer getTotalCustomers() {

		return totalCustomers;
	}

	/**
	 * Sets the total customers.
	 *
	 * @param totalCustomers the totalCustomers to set
	 */
	public void setTotalCustomers(Integer totalCustomers) {

		this.totalCustomers = totalCustomers;
	}

	/**
	 * Gets the total actives customers.
	 *
	 * @return the totalActivesCustomers
	 */
	public Long getTotalActivesCustomers() {

		return totalActivesCustomers;
	}

	/**
	 * Sets the total actives customers.
	 *
	 * @param totalActivesCustomers the totalActivesCustomers to set
	 */
	public void setTotalActivesCustomers(Long totalActivesCustomers) {

		this.totalActivesCustomers = totalActivesCustomers;
	}

	/**
	 * Gets the series amount loans.
	 *
	 * @return the seriesAmountLoans
	 */
	public List<Long> getSeriesAmountLoans() {

		return seriesAmountLoans;
	}

	/**
	 * Sets the series amount loans.
	 *
	 * @param seriesAmountLoans the seriesAmountLoans to set
	 */
	public void setSeriesAmountLoans(List<Long> seriesAmountLoans) {

		this.seriesAmountLoans = seriesAmountLoans;
	}

	/**
	 * Gets the series total customers.
	 *
	 * @return the seriesTotalCustomers
	 */
	public List<Integer> getSeriesTotalCustomers() {

		return seriesTotalCustomers;
	}

	/**
	 * Sets the series total customers.
	 *
	 * @param seriesTotalCustomers the seriesTotalCustomers to set
	 */
	public void setSeriesTotalCustomers(List<Integer> seriesTotalCustomers) {

		this.seriesTotalCustomers = seriesTotalCustomers;
	}

	/**
	 * Gets the series total active customers.
	 *
	 * @return the seriesTotalActiveCustomers
	 */
	public List<Integer> getSeriesTotalActiveCustomers() {

		return seriesTotalActiveCustomers;
	}

	/**
	 * Sets the series total active customers.
	 *
	 * @param seriesTotalActiveCustomers the seriesTotalActiveCustomers to set
	 */
	public void setSeriesTotalActiveCustomers(List<Integer> seriesTotalActiveCustomers) {

		this.seriesTotalActiveCustomers = seriesTotalActiveCustomers;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAnalyticsDTO [" + (total != null ? "total=" + total + ", " : "")
				+ (pourcentage != null ? "pourcentage=" + pourcentage + ", " : "")
				+ (loanNumberByProduct != null ? "loanNumberByProduct=" + loanNumberByProduct + ", "
						: "")
				+ (labelsProducts != null ? "labelsProducts=" + labelsProducts + ", " : "")
				+ (xaxisCategories != null ? "xaxisCategories=" + xaxisCategories + ", " : "")
				+ (seriesAppliedLoans != null ? "seriesAppliedLoans=" + seriesAppliedLoans + ", "
						: "")
				+ (seriesApprovedLoans != null ? "seriesApprovedLoans=" + seriesApprovedLoans + ", "
						: "")
				+ (seriesCanceledRejectedLoans != null
						? "seriesCanceledRejectedLoans=" + seriesCanceledRejectedLoans + ", "
						: "")
				+ (currency != null ? "currency=" + currency + ", " : "")
				+ (totalCustomers != null ? "totalCustomers=" + totalCustomers + ", " : "")
				+ (totalActivesCustomers != null
						? "totalActivesCustomers=" + totalActivesCustomers + ", "
						: "")
				+ (seriesAmountLoans != null ? "seriesAmountLoans=" + seriesAmountLoans + ", " : "")
				+ (seriesTotalCustomers != null
						? "seriesTotalCustomers=" + seriesTotalCustomers + ", "
						: "")
				+ (seriesTotalActiveCustomers != null
						? "seriesTotalActiveCustomers=" + seriesTotalActiveCustomers
						: "")
				+ "]";
	}

}
