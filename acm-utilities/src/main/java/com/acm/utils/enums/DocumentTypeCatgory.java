/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link DocumentTypeCatgory} enum.
 *
 * @author AbdelkarimTurki
 * @since 1.0.1
 */
public enum DocumentTypeCatgory {

	/** The loan. */
	LOAN(0),

	/** The client. */
	CLIENT(1),

	/** The assign document. */
	ASSIGN_DOCUMENT(2),

	/** The collection. */
	COLLECTION(4),

	/** The legal collection. */
	LEGAL(5),

	/** The generic wokflow. */
	GENERIC_WOKFLOW(6),

	/** The supplier. */
	SUPPLIER(7),

	/** The convention. */
	CONVENTION(8),

	/** The third party. */
	THIRD_PARTY(9);

	/** The category id. */
	private Integer categoryId;

	/**
	 * Instantiates a new document type catgory.
	 *
	 * @param categoryId the category id
	 */
	DocumentTypeCatgory(Integer categoryId) {

		this.categoryId = categoryId;
	}

	/**
	 * Category id.
	 *
	 * @return the integer
	 */
	public Integer categoryId() {

		return categoryId;
	}
}
