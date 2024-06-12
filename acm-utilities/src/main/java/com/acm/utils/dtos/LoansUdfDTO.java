/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link LoansUdfDTO} class.
 *
 * @author Ines Dridi
 * @since 1.1.4
 */
public class LoansUdfDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8194182512341333639L;

	/** The loan DTO. */
	private Long loanId;

	/** The account number extern. */
	private String accountNumberExtern;

	/** The udf groupe fields models. */
	private List<UDFLinksGroupeFieldsDTO> acmUdfLinksGroupeFieldDTOs;

	/**
	 * Instantiates a new loans udf DTO.
	 */
	public LoansUdfDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loans udf DTO.
	 *
	 * @param loanId the loan id
	 * @param accountNumberExtern the account number extern
	 * @param acmUdfLinksGroupeFieldDTOs the acm udf links groupe field DT os
	 */
	public LoansUdfDTO(Long loanId, String accountNumberExtern,
			List<UDFLinksGroupeFieldsDTO> acmUdfLinksGroupeFieldDTOs) {

		this.loanId = loanId;
		this.accountNumberExtern = accountNumberExtern;
		this.acmUdfLinksGroupeFieldDTOs = acmUdfLinksGroupeFieldDTOs;
	}

	/**
	 * Gets the loan id.
	 *
	 * @return the loanId
	 */
	public Long getLoanId() {

		return loanId;
	}

	/**
	 * Sets the loan id.
	 *
	 * @param loanId the loanId to set
	 */
	public void setLoanId(Long loanId) {

		this.loanId = loanId;
	}

	/**
	 * Gets the account number extern.
	 *
	 * @return the accountNumberExtern
	 */
	public String getAccountNumberExtern() {

		return accountNumberExtern;
	}

	/**
	 * Sets the account number extern.
	 *
	 * @param accountNumberExtern the accountNumberExtern to set
	 */
	public void setAccountNumberExtern(String accountNumberExtern) {

		this.accountNumberExtern = accountNumberExtern;
	}

	/**
	 * Gets the acm udf links groupe field DT os.
	 *
	 * @return the acmUdfLinksGroupeFieldDTOs
	 */
	public List<UDFLinksGroupeFieldsDTO> getAcmUdfLinksGroupeFieldDTOs() {

		return acmUdfLinksGroupeFieldDTOs;
	}

	/**
	 * Sets the acm udf links groupe field DT os.
	 *
	 * @param acmUdfLinksGroupeFieldDTOs the acmUdfLinksGroupeFieldDTOs to set
	 */
	public void setAcmUdfLinksGroupeFieldDTOs(
			List<UDFLinksGroupeFieldsDTO> acmUdfLinksGroupeFieldDTOs) {

		this.acmUdfLinksGroupeFieldDTOs = acmUdfLinksGroupeFieldDTOs;
	}
}
