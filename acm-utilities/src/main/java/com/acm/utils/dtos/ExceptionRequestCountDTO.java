/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * The Class ExceptionRequestCountDTO.
 */
public class ExceptionRequestCountDTO implements java.io.Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2942768668708233048L;

	/** The count new. */
	Long countNew;

	/** The count accepted. */
	Long countAccepted;

	/** The count rejected. */
	Long countRejected;

	/** The count cancelled. */
	Long countCancelled;

	/** The count closed. */
	Long countClosed;

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

	/**
	 * Gets the count cancelled.
	 *
	 * @return the count cancelled
	 */
	public Long getCountCancelled() {

		return countCancelled;
	}

	/**
	 * Sets the count cancelled.
	 *
	 * @param countCancelled the new count cancelled
	 */
	public void setCountCancelled(Long countCancelled) {

		this.countCancelled = countCancelled;
	}

	/**
	 * Gets the count closed.
	 *
	 * @return the count closed
	 */
	public Long getCountClosed() {

		return countClosed;
	}

	/**
	 * Sets the count closed.
	 *
	 * @param countClosed the new count closed
	 */
	public void setCountClosed(Long countClosed) {

		this.countClosed = countClosed;
	}

	/**
	 * Instantiates a new exception request count DTO.
	 */
	public ExceptionRequestCountDTO() {

		super();
	}
}
