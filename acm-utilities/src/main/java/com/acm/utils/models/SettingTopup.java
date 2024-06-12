/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 * {@link SettingTopup} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_SETTING_TOPUP")
public class SettingTopup extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5258919245782608036L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_TOPUP", unique = true, nullable = false)
	private Long id;

	/** The product id. */
	@OneToOne(mappedBy = "settingTopup", fetch = FetchType.LAZY)
	private Product product;

	/** The product id. */
	@Column(name = "ID_ACM_PRODUCT")
	private Long productId;

	/** The product abacus id. */
	@Column(name = "ID_ABACUS_PRODUCT")
	private Long productAbacusId;
	/** The topup min loan payement pourcentage. */
	@Column(name = "TOPUP_MIN_LOAN_PAYMENT_PERCENTAGE")
	private Float topupMinLoanPaymentPercentage;

	/** The topup max continuous late days. */
	@Column(name = "TOPUP_MAX_CONTINUOUS_LATE_DAYS")
	private Integer topupMaxContinuousLateDays;

	/** The topup max seperate late days. */
	@Column(name = "TOPUP_MAX_SEPARATE_LATE_DAYS")
	private Integer topupMaxSeparateLateDays;

	/** The topup min loan amount type. */
	@Column(name = "TOPUP_MIN_LOAN_AMOUNT_TYPE")
	private Integer topupMinLoanAmountType;

	/** The topup min fixed loan amount. */
	@Column(name = "TOPUP_MIN_FIXED_LOAN_AMOUNT")
	private BigDecimal topupMinFixedLoanAmount;

	/** The topup max allowed topups. */
	@Column(name = "TOPUP_MAX_ALLOWED_TOPUPS")
	private Integer topupMaxAllowedTopups;

	/** The topup min previously issued loans number. */
	@Column(name = "TOPUP_MIN_PREVIOUSLY_ISSUED_LOANS_NUMBER")
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
	 * Gets the product.
	 *
	 * @return the product
	 */
	public Product getProduct() {

		return product;
	}

	/**
	 * Sets the product.
	 *
	 * @param product the new product
	 */
	public void setProduct(Product product) {

		this.product = product;
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

}
