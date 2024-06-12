/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * The Class SimahClassType.
 */
@Entity
@Table(name = "SIMAH_CLASS_TYPE")
public class SimahClassType implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2336216447513682704L;

	/** The id. */
	@Id
	@Column(name = "ID_SIMAH_CLASS_TYPE", unique = true, nullable = false)
	private Long id;

	/** The class label. */
	@Column(name = "CLASS_LABEL", unique = true, nullable = false)
	private String classLabel;

	/** The class description. */
	@Column(name = "CLASS_DESCRIPTION", nullable = false)
	private String classDescription;

	/**
	 * Instantiates a new class type.
	 */
	public SimahClassType() {

	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
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
