/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link LoanDetailsDTO} class. regroupant {@link LoanDTO} && {@link ScheduleDTO} &&
 * {@link CustomerDTO} && {@link CustomerAccountDTO}
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public class LoanDetailsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 5695848961831363435L;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The schedule DT os. */
	private List<ScheduleDTO> scheduleDTOs;

	/** The customer account DT os. */
	private List<CustomerAccountDTO> customerAccountDTOs;

	/**
	 * Instantiates a new loan details DTO.
	 */
	public LoanDetailsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the loan DTO.
	 *
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the schedule DT os.
	 *
	 * @return the scheduleDTOs
	 */
	public List<ScheduleDTO> getScheduleDTOs() {

		return scheduleDTOs;
	}

	/**
	 * Sets the schedule DT os.
	 *
	 * @param scheduleDTOs the scheduleDTOs to set
	 */
	public void setScheduleDTOs(List<ScheduleDTO> scheduleDTOs) {

		this.scheduleDTOs = scheduleDTOs;
	}

	/**
	 * Gets the customer account DT os.
	 *
	 * @return the customerAccountDTOs
	 */
	public List<CustomerAccountDTO> getCustomerAccountDTOs() {

		return customerAccountDTOs;
	}

	/**
	 * Sets the customer account DT os.
	 *
	 * @param customerAccountDTOs the customerAccountDTOs to set
	 */
	public void setCustomerAccountDTOs(List<CustomerAccountDTO> customerAccountDTOs) {

		this.customerAccountDTOs = customerAccountDTOs;
	}
}
