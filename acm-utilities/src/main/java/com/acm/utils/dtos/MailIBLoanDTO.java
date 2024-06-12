/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.acm.utils.enums.MailBuilderMethod;

/**
 * The Class MailIBLoanDTO.
 */
public class MailIBLoanDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1215506279706172686L;

	/** The IBloan DTO. */
	private IBLoanDTO ibLoanDTO;

	/** The mail DTO. */
	private MailDTO mailDTO;

	/** The mail builder method. */
	private MailBuilderMethod mailBuilderMethod;

	/**
	 * Instantiates a new mail IB loan DTO.
	 */
	public MailIBLoanDTO() {

	}

	/**
	 * Instantiates a new mail IB loan DTO.
	 *
	 * @param ibLoanDTO the ib loan DTO
	 * @param mailDTO the mail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	public MailIBLoanDTO(IBLoanDTO ibLoanDTO, MailDTO mailDTO,
			MailBuilderMethod mailBuilderMethod) {

		this.ibLoanDTO = ibLoanDTO;
		this.mailDTO = mailDTO;
		this.mailBuilderMethod = mailBuilderMethod;
	}

	/**
	 * Gets the ib loan DTO.
	 *
	 * @return the ib loan DTO
	 */
	public IBLoanDTO getIbLoanDTO() {

		return ibLoanDTO;
	}

	/**
	 * Sets the ib loan DTO.
	 *
	 * @param ibLoanDTO the new ib loan DTO
	 */
	public void setIbLoanDTO(IBLoanDTO ibLoanDTO) {

		this.ibLoanDTO = ibLoanDTO;
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
	 * Sets the mail DTO.
	 *
	 * @param mailDTO the new mail DTO
	 */
	public void setMailDTO(MailDTO mailDTO) {

		this.mailDTO = mailDTO;
	}

	/**
	 * Gets the mail builder method.
	 *
	 * @return the mail builder method
	 */
	public MailBuilderMethod getMailBuilderMethod() {

		return mailBuilderMethod;
	}

	/**
	 * Sets the mail builder method.
	 *
	 * @param mailBuilderMethod the new mail builder method
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

		return "MailIBLoanDTO [ibLoanDTO=" + ibLoanDTO + ", mailDTO=" + mailDTO
				+ ", mailBuilderMethod=" + mailBuilderMethod + "]";
	}

}
