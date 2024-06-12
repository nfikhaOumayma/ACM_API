/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.enums.MailBuilderMethod;

/**
 * {@link MailLoanDTO } class.
 *
 * @author AbdelkarimTurki
 * @since 0.11.0
 */
public class MailLoanDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2364872260902988183L;

	/** The loan DTO. */
	private LoanDTO loanDTO;

	/** The mail DTO. */
	private MailDTO mailDTO;

	/** The mail builder method. */
	private MailBuilderMethod mailBuilderMethod;

	/** The user connected. */
	private String userConnected;

	/**
	 * Instantiates a new loanMail DTO.
	 */
	public MailLoanDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new loan mail DTO.
	 *
	 * @param loanDTO the loan DTO
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 * @param userConnected the user connected
	 */
	public MailLoanDTO(LoanDTO loanDTO, MailDTO mailDTO, MailBuilderMethod mailBuilderMethod,
			String userConnected) {

		this.loanDTO = loanDTO;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
		this.userConnected = userConnected;
	}

	/**
	 * Gets the loan.
	 * 
	 * @return the loanDTO
	 */
	public LoanDTO getLoanDTO() {

		return loanDTO;
	}

	/**
	 * Sets the loan.
	 * 
	 * @param loanDTO the loanDTO to set
	 */
	public void setLoanDTO(LoanDTO loanDTO) {

		this.loanDTO = loanDTO;
	}

	/**
	 * Gets the mail DTO.
	 *
	 * @return the mail DTO
	 */
	public MailDTO getMailDTO() {

		return mailDTO;
	}

	/**
	 * Sets the loan.
	 * 
	 * @param mailDTO the mailDTO to set
	 */
	public void setMailDTO(MailDTO mailDTO) {

		this.mailDTO = mailDTO;
	}

	/**
	 * Gets the mail builder method.
	 *
	 * @return the mailBuilderMethod
	 */
	public MailBuilderMethod getMailBuilderMethod() {

		return mailBuilderMethod;
	}

	/**
	 * Sets the mail builder method.
	 *
	 * @param mailBuilderMethod the mailBuilderMethod to set
	 */
	public void setMailBuilderMethod(MailBuilderMethod mailBuilderMethod) {

		this.mailBuilderMethod = mailBuilderMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "MailLoanDTO [loanDTO=" + loanDTO + ", mailDTO=" + mailDTO + "]";
	}

	/**
	 * Gets the user connected.
	 *
	 * @return the userConnected
	 */
	public String getUserConnected() {

		return userConnected;
	}

	/**
	 * Sets the user connected.
	 *
	 * @param userConnected the userConnected to set
	 */
	public void setUserConnected(String userConnected) {

		this.userConnected = userConnected;
	}

}
