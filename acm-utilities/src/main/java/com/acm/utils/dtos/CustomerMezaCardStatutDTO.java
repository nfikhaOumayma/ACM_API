/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link CustomerMezaCardStatutDTO} class.
 *
 * @author MoezMhiri
 * @since 1.1.5
 */
public class CustomerMezaCardStatutDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7668624510036185487L;

	/** The sent. */
	private Long sent;

	/** The trusted. */
	private Long trusted;

	/** The untrusted. */
	private Long untrusted;

	/**
	 * Instantiates a new loan statut DTO.
	 */
	public CustomerMezaCardStatutDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Instantiates a new customer meza card statut DTO.
	 *
	 * @param sent the sent
	 * @param trusted the trusted
	 * @param untrusted the untrusted
	 */
	public CustomerMezaCardStatutDTO(Long sent, Long trusted, Long untrusted) {

		this.sent = sent;
		this.trusted = trusted;
		this.untrusted = untrusted;
	}

	/**
	 * Gets the sent.
	 *
	 * @return the sent
	 */
	public Long getSent() {

		return sent;
	}

	/**
	 * Sets the sent.
	 *
	 * @param sent the sent to set
	 */
	public void setSent(Long sent) {

		this.sent = sent;
	}

	/**
	 * Gets the trusted.
	 *
	 * @return the trusted
	 */
	public Long getTrusted() {

		return trusted;
	}

	/**
	 * Sets the trusted.
	 *
	 * @param trusted the trusted to set
	 */
	public void setTrusted(Long trusted) {

		this.trusted = trusted;
	}

	/**
	 * Gets the untrusted.
	 *
	 * @return the untrusted
	 */
	public Long getUntrusted() {

		return untrusted;
	}

	/**
	 * Sets the untrusted.
	 *
	 * @param untrusted the untrusted to set
	 */
	public void setUntrusted(Long untrusted) {

		this.untrusted = untrusted;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerMezaCardStatutDTO [sent=" + sent + ", trusted=" + trusted + ", untrusted="
				+ untrusted + "]";
	}

}
