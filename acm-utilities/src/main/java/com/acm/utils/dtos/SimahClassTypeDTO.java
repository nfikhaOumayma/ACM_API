/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * The Class SimahClassTypeDTO.
 */
public class SimahClassTypeDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5439563102504165838L;

	/** The class label. */
	private String classLabel;

	/** The class description. */
	private String classDescription;

	/**
	 * Instantiates a new class type DTO.
	 */
	public SimahClassTypeDTO() {

	}

	/**
	 * Gets the class label.
	 *
	 * @return the class label
	 */
	public String getClassLabel() {

		return classLabel;
	}

	/**
	 * Sets the class label.
	 *
	 * @param classLabel the new class label
	 */
	public void setClassLabel(String classLabel) {

		this.classLabel = classLabel;
	}

	/**
	 * Gets the class description.
	 *
	 * @return the class description
	 */
	public String getClassDescription() {

		return classDescription;
	}

	/**
	 * Sets the class description.
	 *
	 * @param classDescription the new class description
	 */
	public void setClassDescription(String classDescription) {

		this.classDescription = classDescription;
	}

}
