/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * {@link SettingTopupDTO} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class SettingTopupDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7397594438171939211L;

	/** The id. */
	private Long id;

	/** The product id. */
	private Long productId;

	/** The product abacus id. */
	private Long productAbacusId;
	/** The topup min loan payement pourcentage. */
	private Float topupMinLoanPaymentPercentage;

	/** The topup max continuous late days. */
	private Integer topupMaxContinuousLateDays;

	/** The topup max seperate late days. */
	private Integer topupMaxSeparateLateDays;

	/** The topup min loan amount type. */
	private Integer topupMinLoanAmountType;

	/** The topup min fixed loan amount. */
	private BigDecimal topupMinFixedLoanAmount;

	/** The topup max allowed topups. */
	private Integer topupMaxAllowedTopups;

	/** The topup min previously issued loans number. */
	private Integer topupMinPreviouslyIssuedLoansNumber;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the product abacus id.
	 *
	 * @return the product abacus id
	 */
	public Long getProductAbacusId() {

		return productAbacusId;
	}

	/**
	 * Sets the product abacus id.
	 *
	 * @param productAbacusId the new product abacus id
	 */
	public void setProductAbacusId(Long productAbacusId) {

		this.productAbacusId = productAbacusId;
	}

	/**
	 * Gets the product id.
	 *
	 * @return the product id
	 */
	public Long getProductId() {

		return productId;
	}

	/**
	 * Sets the product id.
	 *
	 * @param productId the new product id
	 */
	public void setProductId(Long productId) {

		this.productId = productId;
	}

	/**
	 * Gets the topup min loan payment percentage.
	 *
	 * @return the topup min loan payment percentage
	 */
	public Float getTopupMinLoanPaymentPercentage() {

		return topupMinLoanPaymentPercentage;
	}

	/**
	 * Sets the topup min loan payment percentage.
	 *
	 * @param topupMinLoanPaymentPercentage the new topup min loan payment percentage
	 */
	public void setTopupMinLoanPaymentPercentage(Float topupMinLoanPaymentPercentage) {

		this.topupMinLoanPaymentPercentage = topupMinLoanPaymentPercentage;
	}

	/**
	 * Gets the topup max continuous late days.
	 *
	 * @return the topup max continuous late days
	 */
	public Integer getTopupMaxContinuousLateDays() {

		return topupMaxContinuousLateDays;
	}

	/**
	 * Sets the topup max continuous late days.
	 *
	 * @param topupMaxContinuousLateDays the new topup max continuous late days
	 */
	public void setTopupMaxContinuousLateDays(Integer topupMaxContinuousLateDays) {

		this.topupMaxContinuousLateDays = topupMaxContinuousLateDays;
	}

	/**
	 * Gets the topup max separate late days.
	 *
	 * @return the topup max separate late days
	 */
	public Integer getTopupMaxSeparateLateDays() {

		return topupMaxSeparateLateDays;
	}

	/**
	 * Sets the topup max separate late days.
	 *
	 * @param topupMaxSeparateLateDays the new topup max separate late days
	 */
	public void setTopupMaxSeparateLateDays(Integer topupMaxSeparateLateDays) {

		this.topupMaxSeparateLateDays = topupMaxSeparateLateDays;
	}

	/**
	 * Gets the topup min loan amount type.
	 *
	 * @return the topup min loan amount type
	 */
	public Integer getTopupMinLoanAmountType() {

		return topupMinLoanAmountType;
	}

	/**
	 * Sets the topup min loan amount type.
	 *
	 * @param topupMinLoanAmountType the new topup min loan amount type
	 */
	public void setTopupMinLoanAmountType(Integer topupMinLoanAmountType) {

		this.topupMinLoanAmountType = topupMinLoanAmountType;
	}

	/**
	 * Gets the topup min fixed loan amount.
	 *
	 * @return the topup min fixed loan amount
	 */
	public BigDecimal getTopupMinFixedLoanAmount() {

		return topupMinFixedLoanAmount;
	}

	/**
	 * Sets the topup min fixed loan amount.
	 *
	 * @param topupMinFixedLoanAmount the new topup min fixed loan amount
	 */
	public void setTopupMinFixedLoanAmount(BigDecimal topupMinFixedLoanAmount) {

		this.topupMinFixedLoanAmount = topupMinFixedLoanAmount;
	}

	/**
	 * Gets the topup max allowed topups.
	 *
	 * @return the topup max allowed topups
	 */
	public Integer getTopupMaxAllowedTopups() {

		return topupMaxAllowedTopups;
	}

	/**
	 * Sets the topup max allowed topups.
	 *
	 * @param topupMaxAllowedTopups the new topup max allowed topups
	 */
	public void setTopupMaxAllowedTopups(Integer topupMaxAllowedTopups) {

		this.topupMaxAllowedTopups = topupMaxAllowedTopups;
	}

	/**
	 * Gets the topup min previously issued loans number.
	 *
	 * @return the topup min previously issued loans number
	 */
	public Integer getTopupMinPreviouslyIssuedLoansNumber() {

		return topupMinPreviouslyIssuedLoansNumber;
	}

	/**
	 * Sets the topup min previously issued loans number.
	 *
	 * @param topupMinPreviouslyIssuedLoansNumber the new topup min previously issued loans number
	 */
	public void setTopupMinPreviouslyIssuedLoansNumber(
			Integer topupMinPreviouslyIssuedLoansNumber) {

		this.topupMinPreviouslyIssuedLoansNumber = topupMinPreviouslyIssuedLoansNumber;
	}

}
