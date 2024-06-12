/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link LoanScheduleDTO} class.
 *
 * @author Ines Dridi
 * @since 0.1.0
 */
public class LoanScheduleDTO extends GenericDTO implements Serializable {
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3889515368753565981L;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The schedule DT os. */
	private List<ScheduleDTO> scheduleDTOs;

	/**
	 * Instantiates a new loan schedule DTO.
	 *
	 * @param loanDTO the loan DTO
	 * @param scheduleDTOs the schedule DT os
	 */
	public LoanScheduleDTO(LoanDTO loanDTO, List<ScheduleDTO> scheduleDTOs) {

		super();
		this.loanDTO = loanDTO;
		this.scheduleDTOs = scheduleDTOs;
	}

	/**
	 * Gets the loan DTO.
	 *
	 * @return the loan DTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan DTO.
	 *
	 * @param loanDTO the new loan DTO
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the schedule DT os.
	 *
	 * @return the schedule DT os
	 */
	public List<ScheduleDTO> getScheduleDTOs() {

		return scheduleDTOs;
	}

	/**
	 * Sets the schedule DT os.
	 *
	 * @param scheduleDTOs the new schedule DT os
	 */
	public void setScheduleDTOs(List<ScheduleDTO> scheduleDTOs) {

		this.scheduleDTOs = scheduleDTOs;
	}

}
