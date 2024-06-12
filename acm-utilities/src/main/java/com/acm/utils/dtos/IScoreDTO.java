/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link I_ScoreDTO} class.
 *
 * @author Yesser
 * @since 1.0.5
 */
public class IScoreDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7398241780006114410L;

	/** The i score loan. */
	private String iScoreLoan;

	/** The i score customer. */
	private String iScoreCustomer;

	/**
	 * Instantiates a new i score DTO.
	 */
	public IScoreDTO() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the i score loan.
	 *
	 * @return the i score loan
	 */
	public String getiScoreLoan() {

		return iScoreLoan;
	}

	/**
	 * Sets the i score loan.
	 *
	 * @param iScoreLoan the new i score loan
	 */
	public void setiScoreLoan(String iScoreLoan) {

		this.iScoreLoan = iScoreLoan;
	}

	/**
	 * Gets the i score customer.
	 *
	 * @return the i score customer
	 */
	public String getiScoreCustomer() {

		return iScoreCustomer;
	}

	/**
	 * Sets the i score customer.
	 *
	 * @param iScoreCustomer the new i score customer
	 */
	public void setiScoreCustomer(String iScoreCustomer) {

		this.iScoreCustomer = iScoreCustomer;
	}

}
