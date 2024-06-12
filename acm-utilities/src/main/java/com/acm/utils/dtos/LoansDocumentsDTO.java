/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.List;

/**
 * {@link LoansDocumentsDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.1.4
 */
public class LoansDocumentsDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4312374109049875138L;

	/** The loan DTO. */
	private Long loanId;

	/** The account number extern. */
	private String accountNumberExtern;

	/** The acm documents DT os. */
	private List<AcmDocumentsDTO> acmDocumentsDTOs;

	/**
	 * Instantiates a new loans documents DTO.
	 */
	public LoansDocumentsDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new loans documents DTO.
	 *
	 * @param loanId the loan id
	 * @param accountNumberExtern the account number extern
	 * @param acmDocumentsDTOs the acm documents DT os
	 */
	public LoansDocumentsDTO(Long loanId, String accountNumberExtern,
			List<AcmDocumentsDTO> acmDocumentsDTOs) {

		this.loanId = loanId;
		this.accountNumberExtern = accountNumberExtern;
		this.acmDocumentsDTOs = acmDocumentsDTOs;
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
	 * Gets the acm documents DT os.
	 *
	 * @return the acmDocumentsDTOs
	 */
	public List<AcmDocumentsDTO> getAcmDocumentsDTOs() {

		return acmDocumentsDTOs;
	}

	/**
	 * Sets the acm documents DT os.
	 *
	 * @param acmDocumentsDTOs the acmDocumentsDTOs to set
	 */
	public void setAcmDocumentsDTOs(List<AcmDocumentsDTO> acmDocumentsDTOs) {

		this.acmDocumentsDTOs = acmDocumentsDTOs;
	}

}
