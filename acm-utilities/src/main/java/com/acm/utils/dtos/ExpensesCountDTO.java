/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link ExpensesCountDTO} class.
 *
 * @author ManelLamloum
 * @since 1.1.3
 */
public class ExpensesCountDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2321760447325503093L;

	/** The count new. */
	Long countNew;

	/** The count accepted. */
	Long countAccepted;

	/** The count rejected. */
	Long countRejected;

	/**
	 * Instantiates a new expenses count DTO.
	 */
	public ExpensesCountDTO() {

		// EMPTY
	}

	/**
	 * Instantiates a new expenses count DTO.
	 *
	 * @param countNew the count new
	 * @param countAccepted the count accepted
	 * @param countRejected the count rejected
	 */
	public ExpensesCountDTO(Long countNew, Long countAccepted, Long countRejected) {

		this.countNew = countNew;
		this.countAccepted = countAccepted;
		this.countRejected = countRejected;
	}

	/**
	 * Gets the count new.
	 *
	 * @return the count new
	 */
	public Long getCountNew() {

		return countNew;
	}

	/**
	 * Sets the count new.
	 *
	 * @param countNew the new count new
	 */
	public void setCountNew(Long countNew) {

		this.countNew = countNew;
	}

	/**
	 * Gets the count accepted.
	 *
	 * @return the count accepted
	 */
	public Long getCountAccepted() {

		return countAccepted;
	}

	/**
	 * Sets the count accepted.
	 *
	 * @param countAccepted the new count accepted
	 */
	public void setCountAccepted(Long countAccepted) {

		this.countAccepted = countAccepted;
	}

	/**
	 * Gets the count rejected.
	 *
	 * @return the count rejected
	 */
	public Long getCountRejected() {

		return countRejected;
	}

	/**
	 * Sets the count rejected.
	 *
	 * @param countRejected the new count rejected
	 */
	public void setCountRejected(Long countRejected) {

		this.countRejected = countRejected;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "ExpensesCountDTO [countNew=" + countNew + ", countAccepted=" + countAccepted
				+ ", countRejected=" + countRejected + "]";
	}

}
