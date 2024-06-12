/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * The Class RenewalConditionLoanDTO.
 */
public class RenewalConditionLoanDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3963930926955358108L;

	/** The paid loans amnout. */
	private List<Long> paidLoansAmnout;

	/** The last loan amount. */
	private Long lastLoanAmount;

	/** The renewal condition DTO. */
	private RenewalConditionDTO renewalConditionDTO;

	/**
	 * Instantiates a new renewal condition loan DTO.
	 */
	public RenewalConditionLoanDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new renewal condition loan DTO.
	 *
	 * @param paidLoansAmnout the paid loans amnout
	 * @param lastLoanAmount the last loan amount
	 * @param renewalConditionDTO the renewal condition DTO
	 */
	public RenewalConditionLoanDTO(List<Long> paidLoansAmnout, Long lastLoanAmount,
			RenewalConditionDTO renewalConditionDTO) {

		this.paidLoansAmnout = paidLoansAmnout;
		this.lastLoanAmount = lastLoanAmount;
		this.renewalConditionDTO = renewalConditionDTO;
	}

	/**
	 * Gets the paid loans amnout.
	 *
	 * @return the paid loans amnout
	 */
	public List<Long> getPaidLoansAmnout() {

		return paidLoansAmnout;
	}

	/**
	 * Sets the paid loans amnout.
	 *
	 * @param paidLoansAmnout the new paid loans amnout
	 */
	public void setPaidLoansAmnout(List<Long> paidLoansAmnout) {

		this.paidLoansAmnout = paidLoansAmnout;
	}

	/**
	 * Gets the last loan amount.
	 *
	 * @return the last loan amount
	 */
	public Long getLastLoanAmount() {

		return lastLoanAmount;
	}

	/**
	 * Sets the last loan amount.
	 *
	 * @param lastLoanAmount the new last loan amount
	 */
	public void setLastLoanAmount(Long lastLoanAmount) {

		this.lastLoanAmount = lastLoanAmount;
	}

	/**
	 * Gets the renewal condition DTO.
	 *
	 * @return the renewal condition DTO
	 */
	public RenewalConditionDTO getRenewalConditionDTO() {

		return renewalConditionDTO;
	}

	/**
	 * Sets the renewal condition DTO.
	 *
	 * @param renewalConditionDTO the new renewal condition DTO
	 */
	public void setRenewalConditionDTO(RenewalConditionDTO renewalConditionDTO) {

		this.renewalConditionDTO = renewalConditionDTO;
	}

}
