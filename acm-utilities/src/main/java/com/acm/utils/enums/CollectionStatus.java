/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The Enum CollectionStatus.
 */
public enum CollectionStatus {

	/** The activated. */
	ACTIVE(0),

	/** The closed. */
	CLOSED(-1),

	/** The completed. */
	COMPLETED(1);

	/** The category id. */
	private Integer statusId;

	/**
	 * Instantiates a new collection status.
	 *
	 * @param statusId the status id
	 */
	CollectionStatus(Integer statusId) {

		this.statusId = statusId;
	}

	/**
	 * Category id.
	 *
	 * @return the integer
	 */
	public Integer statusId() {

		return statusId;
	}
}
