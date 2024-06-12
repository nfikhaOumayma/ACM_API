/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link SettingTopupValidityDTO } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class SettingTopupValidityDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4853288155147260384L;

	/** The min loan payment percentage validity. */
	private Boolean minLoanPaymentPercentageValidity; // TRUE if the rule is valid (OK) ; FALSE if
														// the rule is not respected

	/** The max continuous late days or max separate late days validity. */
	private Integer maxContinuousLateDaysOrMaxSeparateLateDaysValidity; // TRUE if the rule is valid
																		// (OK) ; FALSE if the rule
																		// is not respected

	/** The max allowed topups validity. */
	private Boolean maxAllowedTopupsValidity; // TRUE if the rule is valid (OK) ; FALSE if the rule
												// is not respected

	/** The min previously issued loans number validity. */
	private Boolean minPreviouslyIssuedLoansNumberValidity; // TRUE if the rule is valid (OK) ;
															// FALSE if the rule is not respected

	/**
	 * Gets the min loan payment percentage validity.
	 *
	 * @return the min loan payment percentage validity
	 */
	public Boolean getMinLoanPaymentPercentageValidity() {

		return minLoanPaymentPercentageValidity;
	}

	/**
	 * Sets the min loan payment percentage validity.
	 *
	 * @param minLoanPaymentPercentageValidity the new min loan payment percentage validity
	 */
	public void setMinLoanPaymentPercentageValidity(Boolean minLoanPaymentPercentageValidity) {

		this.minLoanPaymentPercentageValidity = minLoanPaymentPercentageValidity;
	}

	/**
	 * Gets the max continuous late days or max separate late days validity.
	 *
	 * @return the max continuous late days or max separate late days validity
	 */
	public Integer getMaxContinuousLateDaysOrMaxSeparateLateDaysValidity() {

		return maxContinuousLateDaysOrMaxSeparateLateDaysValidity;
	}

	/**
	 * Sets the max continuous late days or max separate late days validity.
	 *
	 * @param maxContinuousLateDaysOrMaxSeparateLateDaysValidity the new max continuous late days or max separate late days validity
	 */
	public void setMaxContinuousLateDaysOrMaxSeparateLateDaysValidity(
			Integer maxContinuousLateDaysOrMaxSeparateLateDaysValidity) {

		this.maxContinuousLateDaysOrMaxSeparateLateDaysValidity =
				maxContinuousLateDaysOrMaxSeparateLateDaysValidity;
	}

	/**
	 * Gets the max allowed topups validity.
	 *
	 * @return the max allowed topups validity
	 */
	public Boolean getMaxAllowedTopupsValidity() {

		return maxAllowedTopupsValidity;
	}

	/**
	 * Sets the max allowed topups validity.
	 *
	 * @param maxAllowedTopupsValidity the new max allowed topups validity
	 */
	public void setMaxAllowedTopupsValidity(Boolean maxAllowedTopupsValidity) {

		this.maxAllowedTopupsValidity = maxAllowedTopupsValidity;
	}

	/**
	 * Gets the min previously issued loans number validity.
	 *
	 * @return the min previously issued loans number validity
	 */
	public Boolean getMinPreviouslyIssuedLoansNumberValidity() {

		return minPreviouslyIssuedLoansNumberValidity;
	}

	/**
	 * Sets the min previously issued loans number validity.
	 *
	 * @param minPreviouslyIssuedLoansNumberValidity the new min previously issued loans number
	 *        validity
	 */
	public void setMinPreviouslyIssuedLoansNumberValidity(
			Boolean minPreviouslyIssuedLoansNumberValidity) {

		this.minPreviouslyIssuedLoansNumberValidity = minPreviouslyIssuedLoansNumberValidity;
	}

}
