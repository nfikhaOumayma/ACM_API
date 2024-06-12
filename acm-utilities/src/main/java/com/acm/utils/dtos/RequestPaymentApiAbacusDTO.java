/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class RequestPaymentApiAbacusDTO.
 */
public class RequestPaymentApiAbacusDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1203990271246366783L;

	/** The cu account id. */
	private Long cuAccountId;

	/** The username. */
	private String username;

	/** The payment from. */
	private String paymentFrom;

	/** The notes. */
	private String notes;

	/**
	 * Instantiates a new request payment api abacus DTO.
	 */
	public RequestPaymentApiAbacusDTO() {

		super();
	}

	/**
	 * Gets the cu account id.
	 *
	 * @return the cu account id
	 */
	public Long getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the new cu account id
	 */
	public void setCuAccountId(Long cuAccountId) {

		this.cuAccountId = cuAccountId;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUsername() {

		return username;
	}

	/**
	 * Sets the username.
	 *
	 * @param username the new username
	 */
	public void setUsername(String username) {

		this.username = username;
	}

	/**
	 * Gets the payment from.
	 *
	 * @return the payment from
	 */
	public String getPaymentFrom() {

		return paymentFrom;
	}

	/**
	 * Sets the payment from.
	 *
	 * @param paymentFrom the new payment from
	 */
	public void setPaymentFrom(String paymentFrom) {

		this.paymentFrom = paymentFrom;
	}

	/**
	 * Gets the notes.
	 *
	 * @return the notes
	 */
	public String getNotes() {

		return notes;
	}

	/**
	 * Sets the notes.
	 *
	 * @param notes the new notes
	 */
	public void setNotes(String notes) {

		this.notes = notes;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "RequestPaymentApiAbacusDTO [cuAccountId=" + cuAccountId + ", username=" + username
				+ ", paymentFrom=" + paymentFrom + ", notes=" + notes + "]";
	}

}
